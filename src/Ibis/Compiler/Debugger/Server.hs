{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Debugger server for Ibis
--
-- The debugger server is a Minecraft 1.16.5 server listening on port 25545, that communicates
-- with WorldServer.
--
-- https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Protocol?oldid=2773082
module Ibis.Compiler.Debugger.Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, get, put, runStateT)

import Data.Binary.Get (getDoublebe, runGetOrFail)
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
import Ibis.Compiler.World (WorldChunk (..))
import Ibis.Compiler.WorldServer (ServerEnv (..), ServerRequest (..))

-- | Raw Minecraft packet
data Packet = Packet
  { packetId :: !Int
  , packetData :: !LBS.ByteString
  }
  deriving (Show, Eq)

-- | Read a wire VarInt.  A @Nothing@ result is a clean peer disconnect;
-- malformed values are rejected instead of spinning forever.
readVarInt :: Socket -> IO (Maybe Int)
readVarInt sock = loop 0 0
 where
  loop val shiftv = do
    bs <- recv sock 1
    if BS.null bs
      then pure Nothing
      else do
        let byte = fromIntegral (BS.head bs) :: Word32
        let value = val .|. ((byte .&. 0x7F) `shiftL` shiftv)

        if (byte .&. 0x80) /= 0
          then
            if shiftv >= 28
              then throwIO (userError "Malformed Minecraft VarInt")
              else loop value (shiftv + 7)
          else pure (Just (fromIntegral value))

readVarIntWithLen :: Socket -> IO (Maybe (Int, Int))
readVarIntWithLen sock = loop 0 0 0
 where
  loop val shiftv count = do
    bs <- recv sock 1
    if BS.null bs
      then pure Nothing
      else do
        let byte = BS.head bs
            value = val .|. (fromIntegral (byte .&. 0x7F) `shiftL` shiftv)
            newCount = count + 1
        if (byte .&. 0x80) /= 0
          then
            if shiftv >= 28
              then throwIO (userError "Malformed Minecraft VarInt")
              else loop value (shiftv + 7) newCount
          else pure (Just (value, newCount))

buildVarInt :: Int -> Builder
buildVarInt = go . fromIntegral
 where
  -- Minecraft VarInts are signed 32-bit values.  Encoding through Word32 is
  -- essential: arithmetic right shifts make a negative Int recurse forever.
  go :: Word32 -> Builder
  go value
    | value .&. complement 0x7F == 0 = word8 (fromIntegral value)
    | otherwise = word8 (fromIntegral ((value .&. 0x7F) .|. 0x80)) <> go (value `shiftR` 7)

sendPacket :: Socket -> Int -> Builder -> IO ()
sendPacket sock pid payload = do
  let packetIdBytes = toLazyByteString (buildVarInt pid)
  let payloadBytes = toLazyByteString payload
  let packetLength = fromIntegral (LBS.length packetIdBytes + LBS.length payloadBytes) :: Int

  let fullPacket =
        buildVarInt packetLength
          <> lazyByteString packetIdBytes
          <> lazyByteString payloadBytes

  sendAll sock (LBS.toStrict (toLazyByteString fullPacket))

readPacket :: Socket -> IO (Maybe Packet)
readPacket sock = do
  packetLength <- readVarInt sock
  case packetLength of
    Nothing -> pure Nothing
    Just len
      | len < 1 || len > maxPacketSize -> throwIO (userError "Invalid Minecraft packet length")
      | otherwise -> do
          packetHeader <- readVarIntWithLen sock
          case packetHeader of
            Nothing -> pure Nothing
            Just (pid, packetDataLength) -> do
              let payloadLen = len - packetDataLength
              unless (payloadLen >= 0) $ throwIO (userError "Invalid Minecraft packet header")
              payload <- recvExact sock payloadLen
              pure $ Packet pid . LBS.fromStrict <$> payload
 where
  maxPacketSize = 2 * 1024 * 1024

recvExact :: Socket -> Int -> IO (Maybe BS.ByteString)
recvExact _ 0 = pure (Just BS.empty)
recvExact sock n = do
  chunk <- recv sock n
  if BS.null chunk
    then pure Nothing
    else
      if BS.length chunk == n
        then pure (Just chunk)
        else do
          rest <- recvExact sock (n - BS.length chunk)
          pure ((chunk <>) <$> rest)

-- -----------------------------------------------------------------------------
-- 2. Debugger Entry Point & TCP Listener
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
-- 4. Connection Handshake
-- -----------------------------------------------------------------------------

-- | Build the exact NBT registry compound for 1.16.5
buildDimensionCodec :: Builder
buildDimensionCodec =
  buildRootNBT "" $
    TagCompound
      [
        ( "minecraft:dimension_type"
        , TagCompound
            [ ("type", TagString "minecraft:dimension_type")
            ,
              ( "value"
              , TagList
                  10
                  [ TagCompound
                      [ ("name", TagString "minecraft:overworld")
                      , ("id", TagInt 0)
                      , ("element", overworldAttributes)
                      ]
                  ]
              )
            ]
        )
      ,
        ( "minecraft:worldgen/biome"
        , TagCompound
            [ ("type", TagString "minecraft:worldgen/biome")
            ,
              ( "value"
              , TagList
                  10
                  [ TagCompound
                      [ ("name", TagString "minecraft:plains")
                      , ("id", TagInt 1)
                      , ("element", plainsBiome)
                      ]
                  ]
              )
            ]
        )
      ]

-- | Active dimension attributes
buildDimensionTag :: Builder
buildDimensionTag = buildRootNBT "" overworldAttributes

overworldAttributes :: NBT
overworldAttributes =
  TagCompound
    [ ("piglin_safe", TagByte 0)
    , ("natural", TagByte 1)
    , ("coordinate_scale", TagFloat 1.0)
    , ("has_skylight", TagByte 1)
    , ("has_ceiling", TagByte 0)
    , ("ambient_light", TagFloat 0.0)
    , ("infiniburn", TagString "minecraft:infiniburn_overworld")
    , ("has_raids", TagByte 1)
    , ("logical_height", TagInt 256)
    , ("respawn_anchor_works", TagByte 0)
    , ("bed_works", TagByte 1)
    , ("ultrawarm", TagByte 0)
    ]

-- | Minimal complete biome entry accepted by the protocol-754 registry codec.
plainsBiome :: NBT
plainsBiome =
  TagCompound
    [ ("precipitation", TagString "rain")
    , ("depth", TagFloat 0.125)
    , ("temperature", TagFloat 0.8)
    , ("scale", TagFloat 0.05)
    , ("downfall", TagFloat 0.4)
    , ("category", TagString "plains")
    ,
      ( "effects"
      , TagCompound
          [ ("sky_color", TagInt 7907327)
          , ("water_fog_color", TagInt 329011)
          , ("fog_color", TagInt 12638463)
          , ("water_color", TagInt 4159204)
          ]
      )
    ]

handleHandshake
  :: Socket
  -> ServerEnv cat c val
  -> TQueue (ServerRequest cat c val)
  -> IO ()
handleHandshake sock env q = do
  mHsPacket <- readPacket sock
  case mHsPacket of
    Just hsPacket | packetId hsPacket == 0x00 -> do
      let nextState = parseHandshakeNextState (LBS.toStrict $ packetData hsPacket)
      case nextState of
        1 -> handleStatusPing sock
        2 -> handleLogin sock env q
        _ -> pure ()
    _ -> pure ()

buildJoinGame :: Builder
buildJoinGame =
  int32BE 0 -- Entity ID (Int)
    <> word8 0 -- Is hardcore (Boolean)
    <> word8 1 -- Game mode (Creative)
    <> int8 (-1) -- Previous Game mode
    <> buildVarInt 1 -- World Count (1 element)
    <> buildIdentifier "minecraft:overworld" -- World Names Array
    <> buildDimensionCodec -- Safe Type-Checked NBT Codec
    <> buildDimensionTag -- Safe Type-Checked NBT Tag
    <> buildIdentifier "minecraft:overworld" -- Dimension Name
    <> int64BE 0 -- Hashed seed
    <> buildVarInt 0 -- Max Players
    <> buildVarInt viewDistance -- View Distance
    <> word8 0 -- Reduced Debug Info
    <> word8 1 -- Enable respawn screen
    <> word8 0 -- Is Debug
    <> word8 1 -- Is Flat
 where
  buildIdentifier :: String -> Builder
  buildIdentifier str = buildVarInt (length str) <> stringUtf8 str

handleLogin
  :: Socket
  -> ServerEnv cat c val
  -> TQueue (ServerRequest cat c val)
  -> IO ()
handleLogin sock env q = do
  mLoginPacket <- readPacket sock
  loginPacket <- maybe (throwIO $ userError "Client disconnected before login") pure mLoginPacket
  unless (packetId loginPacket == 0x00) $ throwIO (userError "Expected Login Start packet")
  username <- either (throwIO . userError) pure $ parseUsername (LBS.toStrict $ packetData loginPacket)

  -- 1. Login Success (0x02)
  -- In Minecraft 1.16.5 (protocol 754), Login Success contains a UUID value:
  -- exactly two big-endian 64-bit words, followed by the username String.
  let uuidBytes = byteString (BS.replicate 16 0)
      usernameStr = buildVarInt (BS.length username) <> byteString username
  sendPacket sock 0x02 (uuidBytes <> usernameStr)

  -- 2. Join Game (0x24)
  sendPacket sock 0x24 buildJoinGame

  -- 3. Update View Position (0x40) -> Center on Chunk (0, 0)
  sendPacket sock 0x40 (buildVarInt 0 <> buildVarInt 0)

  -- 4. Set Spawn Position (0x42)
  -- Packed Position uses the low twelve bits for Y.
  sendPacket sock 0x42 (int64BE 64)

  -- 5. Player Position And Look (0x34) with Teleport ID = 1
  let posAndLookPayload =
        doubleBE 0.0 -- X
          <> doubleBE 64.0 -- Y
          <> doubleBE 0.0 -- Z
          <> floatBE 0.0 -- Yaw
          <> floatBE 0.0 -- Pitch
          <> word8 0x00 -- Flags
          <> buildVarInt 1 -- Teleport ID
  sendPacket sock 0x34 posAndLookPayload

  -- 6. WAIT FOR TELEPORT CONFIRM (Clientbound 0x34 requires Serverbound 0x00 reply!)
  mConfirmPacket <- readPacket sock
  case mConfirmPacket of
    Just confirmPacket | packetId confirmPacket == 0x00 -> putStrLn "[Ibis Debugger] Teleport confirmed by client."
    Nothing -> throwIO (userError "Client disconnected before teleport confirmation")
    _ -> pure ()

  -- 7. Populate the initial client view buffer through WorldServer and retain
  -- that per-connection state for incremental streaming during movement.
  loadedChunks <- streamChunkBuffer sock q (ChunkPos 0 0 0) Set.empty

  putStrLn $ "[Ibis Debugger] Camera reading head attached: " ++ show username
  void $ runStateT (serverLoop sock env q) loadedChunks

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
-- 5. Live Execution Server Loop
-- -----------------------------------------------------------------------------

-- | Protocol-754 server view distance and the radius used for streamed chunks.
viewDistance :: Int
viewDistance = 1

serverLoop
  :: Socket
  -> ServerEnv cat c val
  -> TQueue (ServerRequest cat c val)
  -> StateT (Set.Set ChunkPos) IO ()
serverLoop sock env q = forever $ do
  mPacket <- liftIO $ readPacket sock
  packet <- liftIO $ maybe (throwIO $ userError "Client disconnected") pure mPacket
  -- Protocol 754: 0x11 is Player Position and 0x12 is Player Position And
  -- Look.  The first three fields of both payloads are X, Y and Z doubles.
  -- Minecraft does not ask for chunks: crossing a chunk boundary causes the
  -- server to request one from WorldServer/WorldGen and stream it to the client.
  whenMovement packet $ do
    loadedChunks <- get
    loadedChunks' <-
      liftIO $
        streamChunkAfterMovement sock env q loadedChunks (LBS.toStrict $ packetData packet)
    put loadedChunks'
 where
  whenMovement packet action
    | packetId packet == 0x11 || packetId packet == 0x12 = action
    | otherwise = pure ()

streamChunkAfterMovement
  :: Socket
  -> ServerEnv cat c val
  -> TQueue (ServerRequest cat c val)
  -> Set.Set ChunkPos
  -> BS.ByteString
  -> IO (Set.Set ChunkPos)
streamChunkAfterMovement sock env q loadedChunks payload = do
  (x, _y, z) <- either (throwIO . userError) pure $ parsePosition payload
  let nextChunk = ChunkPos (floor $ x / 16.0) 0 (floor $ z / 16.0)
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

-- | Populate every chunk in the same square radius advertised in Join Game.
-- Every value is requested from WorldServer; WorldGen remains behind that
-- server's `FetchChunk` boundary and is never invoked by the debugger.
streamChunkBuffer
  :: forall cat (c :: cat) val
   . Socket
  -> TQueue (ServerRequest cat c val)
  -> ChunkPos
  -> Set.Set ChunkPos
  -> IO (Set.Set ChunkPos)
streamChunkBuffer sock q center previouslyLoaded = do
  let desired = chunkBuffer center
      entering = desired `Set.difference` previouslyLoaded
  mapM_ sendAt (Set.toList entering)
  pure desired
 where
  sendAt position = do
    worldChunk <- requestWorldChunk q position
    sendWorldChunk sock worldChunk

chunkBuffer :: ChunkPos -> Set.Set ChunkPos
chunkBuffer (ChunkPos centerX _ centerZ) =
  Set.fromList . map toChunkPos $
    [ (offsetX, offsetZ)
    | offsetZ <- [-viewDistance .. viewDistance]
    , offsetX <- [-viewDistance .. viewDistance]
    ]
 where
  signedX = fromIntegral (fromIntegral centerX :: Int32) :: Int
  signedZ = fromIntegral (fromIntegral centerZ :: Int32) :: Int
  toChunkPos (offsetX, offsetZ) =
    ChunkPos (fromIntegral $ signedX + offsetX) 0 (fromIntegral $ signedZ + offsetZ)

parseHandshakeNextState :: BS.ByteString -> Int
parseHandshakeNextState bs = either (const 0) id $ do
  (_, rest) <- decodeVarInt bs
  (_, rest') <- decodeString rest
  (_, rest'') <- decodeWord16 rest'
  fst <$> decodeVarInt rest''

parseUsername :: BS.ByteString -> Either String BS.ByteString
parseUsername = fmap fst . decodeString

parsePosition :: BS.ByteString -> Either String (Double, Double, Double)
parsePosition bs =
  case runGetOrFail ((,,) <$> getDoublebe <*> getDoublebe <*> getDoublebe) (LBS.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, position) -> Right position

decodeVarInt :: BS.ByteString -> Either String (Int, BS.ByteString)
decodeVarInt = go 0 0
 where
  go value shiftv input =
    case BS.uncons input of
      Nothing -> Left "Truncated VarInt"
      Just (byte, rest)
        | shiftv >= 35 -> Left "VarInt exceeds five bytes"
        | byte .&. 0x80 /= 0 -> go (value .|. (fromIntegral (byte .&. 0x7f) `shiftL` shiftv)) (shiftv + 7) rest
        | otherwise -> Right (value .|. (fromIntegral byte `shiftL` shiftv), rest)

decodeString :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
decodeString input = do
  (len, rest) <- decodeVarInt input
  if len < 0 || len > BS.length rest
    then Left "Invalid Minecraft string length"
    else Right (BS.take len rest, BS.drop len rest)

decodeWord16 :: BS.ByteString -> Either String (Int, BS.ByteString)
decodeWord16 input
  | BS.length input < 2 = Left "Truncated unsigned short"
  | otherwise = Right (fromIntegral (BS.index input 0) `shiftL` 8 .|. fromIntegral (BS.index input 1), BS.drop 2 input)

-- | Request a chunk solely through WorldServer.  WorldServer is responsible
-- for looking up or generating the value with its configured WorldGen action.
requestWorldChunk
  :: forall cat (c :: cat) val
   . TQueue (ServerRequest cat c val)
  -> ChunkPos
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
formatChunkPos (ChunkPos x y z) =
  "ChunkPos " ++ show (asSigned x) ++ " " ++ show (asSigned y) ++ " " ++ show (asSigned z)
 where
  asSigned :: Word32 -> Int32
  asSigned = fromIntegral

-- | Convert the exact WorldChunk returned by WorldServer into Minecraft 1.16.5
-- Chunk Data (clientbound 0x20), then write that packet to the client.
sendWorldChunk :: Socket -> WorldChunk cat c val -> IO ()
sendWorldChunk sock worldChunk =
  sendPacket sock 0x20 (encodeWorldChunk worldChunk)

encodeWorldChunk :: WorldChunk cat c val -> Builder
encodeWorldChunk (WorldChunk (ChunkPos cx 0 cz) _sieve _) =
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
  -- A solid stone section at y=48..63 provides a safe surface at y=64,
  -- where the login teleport places the player.
  primaryBitMask = buildVarInt 0x0008
  sectionData =
    int16BE 4096 -- Non-air block count
      <> word8 4 -- Bits per block
      <> buildVarInt 1 -- Palette size
      <> buildVarInt 1 -- Global block state: minecraft:stone
      <> buildVarInt 256 -- 4096 four-bit entries, packed into 256 longs
      <> mconcat (replicate 256 (int64BE 0))

  -- Motion Blocking Heightmap (1024 longs packed into 36 Int64s)
  heightmapsNbt =
    buildRootNBT "" $
      TagCompound
        [("MOTION_BLOCKING", TagLongArray (packedHeightMap 64))]

  -- Protocol 754 encodes the full-chunk biome array as 1024 VarInts.  Using
  -- 32-bit integers leaves unread bytes in the client packet decoder.
  biomesArray = buildVarInt 1024 <> mconcat (replicate 1024 (buildVarInt 1))

  -- Block Count (2), bits/block, palette length, palette item, data-array
  -- length (VarInt 256 takes two bytes), then 256 longs.
  dataLength = 2 + 1 + 1 + 1 + 2 + 256 * 8
--
encodeWorldChunk _ = error "encodeWorldChunk: Only supports chunks at y=0 for now."

-- | The heightmap stores 256 nine-bit heights, seven values per 64-bit word.
packedHeightMap :: Int64 -> [Int64]
packedHeightMap height =
  replicate 36 $
    fromIntegral (foldr (\index word -> word .|. (fromIntegral height `shiftL` (index * 9))) (0 :: Word64) [0 .. 6])