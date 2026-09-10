{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Debugger server for Ibis
--
-- The debugger server is a Minecraft 1.16.5 server listening on port 25545, that communicates
-- with WorldServer.
--
-- https://minecraft.wiki/w/Protocol?oldid=2772660#Join_Game
module Ibis.Compiler.Debugger.Server where

import Network.Socket

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch, finally, throwIO)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, get, put, runStateT)

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32, Int64)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

import Data.Word (Word32, Word64)
import System.Timeout (timeout)

import Ibis.AST.CoAST (ChunkPos (..))
import Ibis.Compiler.Debugger.NBT
import Ibis.Compiler.Debugger.Protocol

import Ibis.Compiler.World (WorldChunk (..))
import Ibis.Compiler.WorldServer (ServerEnv (..), ServerRequest (..))

-- -----------------------------------------------------------------------------
-- Debugger Entry Point & TCP Listener
-- -----------------------------------------------------------------------------

startDebugger :: ServerEnv cat c val -> TQueue (ServerRequest cat c val) -> IO ()
startDebugger env requestQueue = do
  addr <- resolve "25545"
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 10

  putStrLn "[Ibis Debugger] Visual debugger bound to 0.0.0.0:25545..."

  forever $ do
    (clientSock, _) <- accept sock
    _ <- forkIO $ handleConnection clientSock env requestQueue
    pure ()
 where
  resolve port = do
    let hints = defaultHints{addrFlags = [AI_PASSIVE], addrSocketType = Stream}
    result <- getAddrInfo (Just hints) Nothing (Just port)
    maybe (throwIO $ userError "No TCP address available") pure (listToMaybe result)

handleConnection :: Socket -> ServerEnv cat c val -> TQueue (ServerRequest cat c val) -> IO ()
handleConnection sock env q =
  handleHandshake sock env q `catch` reportDisconnect `finally` close sock
 where
  reportDisconnect :: SomeException -> IO ()
  reportDisconnect err = putStrLn $ "[Ibis Debugger] Client disconnected: " ++ show err

-- -----------------------------------------------------------------------------
-- Connection Handshake
-- -----------------------------------------------------------------------------

handleHandshake
  :: Socket
  -> ServerEnv cat c val
  -> TQueue (ServerRequest cat c val)
  -> IO ()
handleHandshake sock env q = do
  mHsPacket <- readPacket sock
  case mHsPacket of
    Just hsPacket | packetId hsPacket == 0x00 -> do
      case parseHandshakeNextState (LBS.toStrict $ packetData hsPacket) of
        Right 1 -> handleStatusPing sock
        Right 2 -> handleLogin sock env q
        _ -> pure ()
    _ -> pure ()

handleLogin
  :: Socket
  -> ServerEnv cat c val
  -> TQueue (ServerRequest cat c val)
  -> IO ()
handleLogin sock env q = do
  mLoginPacket <- readPacket sock
  loginPacket <- maybe (throwIO $ userError "Client disconnected before login") pure mLoginPacket
  unless (packetId loginPacket == 0x00) $ throwIO (userError "Expected Login Start packet")
  username <- either (throwIO . userError) pure $ parseLoginUsername (LBS.toStrict $ packetData loginPacket)

  -- 1. Login Success (0x02)
  -- In Minecraft 1.16.5 (protocol 754), Login Success contains a UUID value:
  -- exactly two big-endian 64-bit words, followed by the username String.
  let uuidBytes = byteString (BS.replicate 16 0)
      usernameStr = buildVarInt (BS.length username) <> byteString username
  sendPacket sock 0x02 (uuidBytes <> usernameStr)

  -- 2. Join Game (0x24)
  sendPacket
    sock
    0x24
    ( buildMinecraftPacket
        ( JoinGame
            { entityId = 0
            , isHardcore = False
            , gameMode = 1
            , previousGameMode = -1
            , worldCount = 1
            , worldNames = ["minecraft:overworld"]
            , dimensionCodec = buildDimensionCodec
            , dimensionType = buildDimensionTag
            , dimensionName = "minecraft:overworld"
            , hashedSeed = 0
            , maxPlayers = 1
            , viewDistance = fromIntegral serverViewDistance
            , reducedDebugInfo = True
            , enableRespawnScreen = True
            , isDebug = False
            , isFlat = True
            }
        )
    )

  -- 3. Update View Position (0x40) -> Center on Chunk (0, 0)
  sendPacket sock 0x40 (buildVarInt 0 <> buildVarInt 0)

  -- 4. Set Spawn Position (0x42)
  sendPacket
    sock
    0x42
    ( buildMinecraftPacket
        (SetSpawnPosition 0 64 0)
    )

  -- 5. Player Position And Look (0x34) with Teleport ID = 1
  let posAndLookPayload =
        buildMinecraftPacket
          ( PositionAndLook
              { x = 0.0
              , y = 64.0
              , z = 0.0
              , yaw = 0.0
              , pitch = 0.0
              , flags = 0x00
              , teleportId = 1
              }
          )
  sendPacket sock 0x34 posAndLookPayload

  -- 6. WAIT FOR TELEPORT CONFIRM (Clientbound 0x34 requires Serverbound 0x00 reply!)
  mConfirmPacket <- readMinecraftPacket sock
  case mConfirmPacket of
    Just TeleportConfirmed{teleportId = 1} -> pure ()
    Nothing -> throwIO $ userError "Client disconnected before teleport confirmation"
    Just _ -> pure ()

  -- 7. Populate the initial client view buffer through WorldServer and retain
  -- that per-connection state for incremental streaming during movement.
  -- The player spawns at Y=64, immediately above section 3 (blocks 48..63).
  loadedChunks <- streamChunkBuffer sock q (ChunkPos 0 3 0) Set.empty

  putStrLn $ "[Ibis Debugger] Camera reading head attached: " ++ show username
  withKeepAlives sock $ void $ runStateT (serverLoop sock env q) loadedChunks

-- | A 1.16.5 client disconnects if the server is silent for roughly twenty
-- seconds.  Keep-alives begin only after the connection enters play state.
withKeepAlives :: Socket -> IO a -> IO a
withKeepAlives sock = bracket (forkIO $ keepAliveLoop 0) killThread . const
 where
  keepAliveLoop :: Int64 -> IO ()
  keepAliveLoop keepAliveId = do
    threadDelay 15000000
    sendPacket sock 0x1F (int64BE keepAliveId)
    keepAliveLoop (keepAliveId + 1)

handleStatusPing :: Socket -> IO ()
handleStatusPing sock = do
  let jsonResp = "{\"version\":{\"name\":\"Ibis 1.16.5\",\"protocol\":754},\"players\":{\"max\":1,\"online\":1},\"description\":{\"text\":\"Ibis Compiler Topos Debugger\"}}"
      payload = buildVarInt (BS.length jsonResp) <> byteString jsonResp
  sendPacket sock 0x00 payload
  mPingPacket <- readPacket sock
  case mPingPacket of
    Just pingPacket | packetId pingPacket == 0x01 -> sendPacket sock 0x01 (lazyByteString $ packetData pingPacket)
    _ -> pure ()

-- -----------------------------------------------------------------------------
-- Server Loop
-- -----------------------------------------------------------------------------

-- | Server view distance and the radius used for streamed chunks.
serverViewDistance :: Int
serverViewDistance = 1

-- | WorldServer chunks are 16³ sections, while a Minecraft 1.16.5 Chunk Data
-- packet is a complete 16×256×16 column.  Keep a small section radius around
-- the player in the WorldServer request buffer.
serverVerticalViewDistance :: Int
serverVerticalViewDistance = 1

serverLoop
  :: Socket
  -- ^ The client socket to send chunk data to
  -> ServerEnv cat c val
  -- ^ The WorldServer environment for fetching chunks from our Grothendieck site
  -> TQueue (ServerRequest cat c val)
  -> StateT (Set.Set ChunkPos) IO ()
serverLoop sock env q = forever $ do
  packet <- liftIO $ readMinecraftPacket sock >>= maybe (throwIO $ userError "Client disconnected") pure
  case packet of
    PlayerPosition{playerX = currentX, playerY = currentY, playerZ = currentZ} ->
      updateLoadedBuffer currentX currentY currentZ
    PlayerPositionAndLook{playerX = currentX, playerY = currentY, playerZ = currentZ} ->
      updateLoadedBuffer currentX currentY currentZ
    ChatReceived{chatMessage = text} -> do
      liftIO $ putStrLn $ "[Ibis Debugger] Chat message from client: " ++ show text
      liftIO $ sendPacket sock 0x0E (buildMinecraftPacket $ systemChatMessage text)
    TeleportConfirmed{} -> pure ()
    ClientSettings -> pure ()
    UnrecognisedPacket{} -> pure ()
    _ -> pure ()
 where
  updateLoadedBuffer :: Double -> Double -> Double -> StateT (Set.Set ChunkPos) IO ()
  updateLoadedBuffer currentX currentY currentZ = do
    loadedChunks <- get
    loadedChunks' <- liftIO $ streamChunkAfterMovement sock env q loadedChunks currentX currentY currentZ
    put loadedChunks'

streamChunkAfterMovement
  :: Socket
  -- ^ The client socket to send chunk data to
  -> ServerEnv cat c val
  -- ^ The WorldServer environment for fetching chunks from our Grothendieck site
  -> TQueue (ServerRequest cat c val)
  -> Set.Set ChunkPos
  -- ^ The set of chunks currently loaded in the client view buffer
  -> Double
  -- ^ The current world X coordinate of the player
  -> Double
  -- ^ The current world Y coordinate of the player
  -> Double
  -- ^ The current world Z coordinate of the player
  -> IO (Set.Set ChunkPos)
streamChunkAfterMovement sock env q loadedChunks worldX worldY worldZ = do
  let nextChunk =
        ChunkPos
          (floor $ worldX / 16.0)
          (floor $ worldY / 16.0)
          (floor $ worldZ / 16.0)

  changedChunk <- atomically $ do
    currentChunk <- readTVar (serverCursor env)
    if currentChunk == nextChunk
      then pure False
      else writeTVar (serverCursor env) nextChunk >> pure True
  if changedChunk
    then do
      let ChunkPos chunkX _ chunkZ = nextChunk
      sendPacket
        sock
        0x40
        ( buildVarInt (fromIntegral chunkX)
            <> buildVarInt (fromIntegral chunkZ)
        )
      streamChunkBuffer sock q nextChunk loadedChunks
    else pure loadedChunks

-- | Populate each chunk entering the protocol-754 view buffer. Every value is
-- requested from WorldServer; WorldGen remains behind that server's
-- `FetchChunk` boundary and is never invoked by the debugger.
streamChunkBuffer
  :: forall cat (c :: cat) val
   . Socket
  -- ^ The client socket to send chunk data to
  -> TQueue (ServerRequest cat c val)
  -- ^ The WorldServer request queue for fetching chunks
  -> ChunkPos
  -- ^ The center chunk position to stream around
  -> Set.Set ChunkPos
  -- ^ The set of chunks currently loaded in the client view buffer
  -> IO (Set.Set ChunkPos)
streamChunkBuffer sock q center previouslyLoaded = do
  let desired = chunkBuffer center
      entering = desired `Set.difference` previouslyLoaded
      ChunkPos _ centerY _ = center
  mapM_ (requestAndMaybeSend centerY) (Set.toList entering)
  pure desired
 where
  requestAndMaybeSend centerY position@(ChunkPos _ sectionY _) = do
    worldChunk <- requestWorldChunk q position
    -- One clientbound Chunk Data packet represents an X/Z column.  The server
    -- requests every nearby Y section from WorldServer, but sends one column
    -- for each entering X/Z coordinate until the encoder can aggregate all
    -- returned sections into a single full column.
    when (sectionY == centerY) $ sendWorldChunk sock worldChunk

chunkBuffer :: ChunkPos -> Set.Set ChunkPos
chunkBuffer (ChunkPos centerX centerY centerZ) =
  Set.fromList . map toChunkPos $
    [ (offsetX, sectionY, offsetZ)
    | offsetZ <- [-serverViewDistance .. serverViewDistance]
    , sectionY <- verticalSections centerY
    , offsetX <- [-serverViewDistance .. serverViewDistance]
    ]
 where
  signedX = fromIntegral (fromIntegral centerX :: Int32) :: Int
  signedZ = fromIntegral (fromIntegral centerZ :: Int32) :: Int
  toChunkPos (offsetX, sectionY, offsetZ) =
    ChunkPos (fromIntegral $ signedX + offsetX) sectionY (fromIntegral $ signedZ + offsetZ)

-- | The vanilla 1.16.5 overworld has sixteen chunk sections (Y 0 through 15).
-- Clamping avoids constructing negative Word32 section coordinates near bedrock.
verticalSections :: Word32 -> [Word32]
verticalSections centerY =
  [ fromIntegral sectionY
  | sectionY <- [lowerBound .. upperBound]
  ]
 where
  center = fromIntegral centerY :: Int
  lowerBound = max 0 (center - serverVerticalViewDistance)
  upperBound = min 15 (center + serverVerticalViewDistance)

-- | Request a chunk solely through WorldServer.  WorldServer is responsible
-- for looking up or generating the value with its configured WorldGen action.
requestWorldChunk
  :: forall cat (c :: cat) val
   . TQueue (ServerRequest cat c val)
  -- ^ The WorldServer request queue for fetching chunks
  -> ChunkPos
  -- ^ The chunk position to request
  -> IO (WorldChunk cat c val)
requestWorldChunk q pos = do
  replyVar <- newEmptyTMVarIO
  atomically $ writeTQueue q (FetchChunk pos replyVar)
  putStrLn $ "[Ibis Debugger] Requesting chunk at " ++ formatChunkPos pos ++ " from WorldServer..."

  mChunk <- timeout 50000000 (atomically $ takeTMVar replyVar) -- 50 seconds
  maybe (throwIO $ userError "World server did not reply within fifty seconds") pure mChunk

-- | `ChunkPos` stores coordinates as Word32, while Minecraft protocol 754
-- uses signed 32-bit chunk coordinates.  Preserve the raw two's-complement
-- values for WorldServer, but print their signed protocol interpretation.
formatChunkPos :: ChunkPos -> String
formatChunkPos (ChunkPos coordX coordY coordZ) =
  "ChunkPos " ++ show (asSigned coordX) ++ " " ++ show (asSigned coordY) ++ " " ++ show (asSigned coordZ)
 where
  asSigned :: Word32 -> Int32
  asSigned = fromIntegral

-- | Convert the exact WorldChunk returned by WorldServer into Minecraft 1.16.5
-- Chunk Data (clientbound 0x20), then write that packet to the client.
sendWorldChunk :: Socket -> WorldChunk cat c val -> IO ()
sendWorldChunk sock worldChunk =
  sendPacket sock 0x20 (encodeWorldChunk worldChunk)

-- | Encode a WorldChunk into the Minecraft 1.16.5 Chunk Data packet format.
--
-- The wire packet has no chunk-Y coordinate: it describes an X/Z column whose
-- primary bitmask selects the included 16³ sections.  Each returned WorldChunk
-- contributes its own Y section to that column.
encodeWorldChunk :: WorldChunk cat c val -> Builder
encodeWorldChunk (WorldChunk (ChunkPos cx sectionY cz) _sieve _) =
  int32BE (fromIntegral cx) -- Chunk X
    <> int32BE (fromIntegral cz) -- Chunk Z
    <> word8 1 -- Full Chunk (True)
    <> primaryBitMask
    <> heightmapsNbt
    <> biomesArray
    <> buildVarInt dataLength
    <> sectionData
    <> buildVarInt 0 -- Block entities count (0)
 where
  sectionIndex = fromIntegral sectionY :: Int
  primaryBitMask = buildVarInt (1 `shiftL` sectionIndex)
  sectionData =
    int16BE 4096 -- Non-air block count
      <> word8 4 -- Bits per block
      <> buildVarInt 1 -- Palette size
      <> buildVarInt 1 -- Global block state: minecraft:stone
      <> buildVarInt 256 -- 4096 four-bit entries, packed into 256 longs
      <> mconcat (replicate 256 (int64BE 0))

  -- Motion Blocking Heightmap (1024 longs packed into 36 Int64s).  The top of
  -- the generated section is the highest solid block in this column.
  heightmapsNbt =
    buildRootNBT "" $
      TagCompound
        [("MOTION_BLOCKING", TagLongArray (packedHeightMap $ fromIntegral ((sectionIndex + 1) * 16)))]

  -- Protocol 754 encodes the full-chunk biome array as 1024 VarInts.  Using
  -- 32-bit integers leaves unread bytes in the client packet decoder.
  biomesArray = buildVarInt 1024 <> mconcat (replicate 1024 (buildVarInt 1))

  -- Block Count (2), bits/block, palette length, palette item, data-array
  -- length (VarInt 256 takes two bytes), then 256 longs.
  dataLength = 2 + 1 + 1 + 1 + 2 + 256 * 8

  -- Packed heightmap for a flat world at y=64. Each long contains 7 9-bit height values.
  packedHeightMap :: Int64 -> [Int64]
  packedHeightMap height =
    replicate 36 $
      fromIntegral (foldr (\index word -> word .|. (fromIntegral height `shiftL` (index * 9))) (0 :: Word64) [0 .. 6])
