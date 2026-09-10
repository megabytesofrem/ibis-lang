{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- https://minecraft.wiki/w/Protocol?oldid=2772660#Join_Game
module Ibis.Compiler.Debugger.Protocol where

import Control.Exception (throwIO)
import Control.Monad (unless)

import Data.Binary.Get (getDoublebe, runGetOrFail)
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32, Int64, Int8)
import Data.Word (Word32, Word8)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Ibis.Compiler.Debugger.NBT (NBT (..), buildRootNBT)

-- | Raw Minecraft packet
data Packet = Packet
  { packetId :: !Int
  , packetData :: !LBS.ByteString
  }
  deriving (Show, Eq)

data MinecraftPacket
  = -- 0x24: Join Game (Clientbound)
    JoinGame
      { entityId :: !Int32
      , isHardcore :: !Bool
      , gameMode :: !Word8
      , previousGameMode :: !Int8
      , worldCount :: !Int
      , worldNames :: ![BS.ByteString]
      , -- Dimension information
        dimensionCodec :: Builder -- NBT
      , dimensionType :: Builder -- NBT
      , dimensionName :: !BS.ByteString
      , hashedSeed :: !Int64
      , maxPlayers :: !Int
      , viewDistance :: !Int
      , reducedDebugInfo :: !Bool
      , enableRespawnScreen :: !Bool
      , isDebug :: !Bool
      , isFlat :: !Bool
      }
  | -- 0x34: Player Position and Look (Serverbound)
    PositionAndLook
      { x :: !Double
      , y :: !Double
      , z :: !Double
      , yaw :: !Float
      , pitch :: !Float
      , flags :: !Word8
      , teleportId :: !Int
      }
  | -- 0x42: Set Spawn Position (Clientbound)
    SetSpawnPosition
      { spawnX :: !Int32
      , spawnY :: !Int32
      , spawnZ :: !Int32
      }
  | -- 0x00: Teleport Confirm (Serverbound)
    TeleportConfirm {teleportId :: !Int}
  | -- 0x0E: Chat Message (Clientbound, protocol 754)
    ChatMessage {message :: !BS.ByteString}
  | -- 0x00: Teleport Confirm (Serverbound, play)
    TeleportConfirmed {teleportId :: !Int}
  | -- 0x03: Chat Message (Serverbound, play, protocol 754)
    ChatReceived {chatMessage :: !BS.ByteString}
  | -- 0x11: Player Position (Serverbound, play, protocol 754)
    PlayerPosition
      { playerX :: !Double
      , playerY :: !Double
      , playerZ :: !Double
      }
  | -- 0x12: Player Position And Look (Serverbound, play, protocol 754)
    PlayerPositionAndLook
      { playerX :: !Double
      , playerY :: !Double
      , playerZ :: !Double
      }
  | -- 0x04: Client Settings (Serverbound, play)
    ClientSettings
  | -- Unrecognized packet, for debugging purposes.
    -- Contains the packet ID and LBS.ByteString payload.
    UnrecognisedPacket !Int !LBS.ByteString

buildMinecraftPacket :: MinecraftPacket -> Builder
buildMinecraftPacket (JoinGame{..}) =
  int32BE entityId
    -- Protocol 754 (Minecraft 1.16.5) added this Boolean before Game Mode.
    <> word8 (if isHardcore then 1 else 0)
    <> word8 gameMode
    <> int8 previousGameMode
    <> buildVarInt worldCount
    -- The World Names array contains Minecraft Strings (VarInt length), not
    -- NBT strings (unsigned 16-bit length).  The latter shifts the decoder
    -- and makes it interpret NBT bytes as an impossibly large field.
    <> mconcat (map buildMinecraftString worldNames)
    <> dimensionCodec
    <> dimensionType
    <> buildMinecraftString dimensionName
    <> int64BE hashedSeed
    <> buildVarInt maxPlayers
    <> buildVarInt viewDistance
    <> word8 (if reducedDebugInfo then 1 else 0)
    <> word8 (if enableRespawnScreen then 1 else 0)
    <> word8 (if isDebug then 1 else 0)
    <> word8 (if isFlat then 1 else 0)
buildMinecraftPacket (PositionAndLook{..}) =
  doubleBE x
    <> doubleBE y
    <> doubleBE z
    <> floatBE yaw
    <> floatBE pitch
    <> word8 flags
    <> buildVarInt teleportId
buildMinecraftPacket (SetSpawnPosition{..}) =
  int64BE (packPosition spawnX spawnY spawnZ)
buildMinecraftPacket (TeleportConfirm{..}) =
  buildVarInt teleportId
buildMinecraftPacket (ChatMessage{..}) =
  buildMinecraftString message
    <> word8 1 -- System-message position
    <> byteString (BS.replicate 16 0) -- Sender UUID
buildMinecraftPacket packet =
  error $ "Cannot serialize serverbound packet: " ++ showPacketTag packet

showPacketTag :: MinecraftPacket -> String
showPacketTag TeleportConfirmed{} = "TeleportConfirmed"
showPacketTag ChatReceived{} = "ChatReceived"
showPacketTag PlayerPosition{} = "PlayerPosition"
showPacketTag PlayerPositionAndLook{} = "PlayerPositionAndLook"
showPacketTag ClientSettings = "ClientSettings"
showPacketTag UnrecognisedPacket{} = "UnrecognisedPacket"
showPacketTag _ = "clientbound packet"

-- | A protocol-754 String is a UTF-8 byte sequence prefixed by a VarInt.
buildMinecraftString :: BS.ByteString -> Builder
buildMinecraftString value = buildVarInt (BS.length value) <> byteString value

-- | Build a JSON text component for a clientbound system chat message.
-- Escape all JSON control characters because serverbound chat is raw input.
systemChatMessage :: BS.ByteString -> MinecraftPacket
systemChatMessage input = ChatMessage $ "{\"text\":\"" <> escapeJsonString input <> "\"}"

-- | Minecraft 1.16.5 Position: X occupies bits 63..38, Z bits 37..12, and
-- Y the low twelve bits.  Each component is signed and stored in two's
-- complement, so masking preserves negative coordinates correctly.
packPosition :: Int32 -> Int32 -> Int32 -> Int64
packPosition x y z =
  ((fromIntegral x .&. 0x3ffffff) `shiftL` 38)
    .|. ((fromIntegral z .&. 0x3ffffff) `shiftL` 12)
    .|. (fromIntegral y .&. 0xfff)

readMinecraftPacket :: Socket -> IO (Maybe MinecraftPacket)
readMinecraftPacket sock = do
  mPacket <- readPacket sock
  pure $ decodePlayPacket <$> mPacket

-- | Packet framing failure is represented by `Nothing` from `readPacket`.
-- A malformed or unsupported play payload must not be treated as EOF: clients
-- legitimately emit packets this minimal server does not model yet.
decodePlayPacket :: Packet -> MinecraftPacket
decodePlayPacket (Packet packetId payload) =
  case packetId of
    0x00 -> maybe unknown TeleportConfirmed (decodeVarInt payload)
    0x03 -> maybe unknown ChatReceived (decodeMinecraftString payload)
    0x05 -> ClientSettings
    0x11 -> maybe unknown id (decodePosition PlayerPosition payload)
    0x12 -> maybe unknown id (decodePosition PlayerPositionAndLook payload)
    _ -> unknown
 where
  unknown = UnrecognisedPacket packetId payload

decodePosition
  :: (Double -> Double -> Double -> MinecraftPacket)
  -> LBS.ByteString
  -> Maybe MinecraftPacket
decodePosition constructor payload = do
  (remaining, _, (positionX, positionY, positionZ)) <-
    either (const Nothing) Just $
      runGetOrFail ((,,) <$> getDoublebe <*> getDoublebe <*> getDoublebe) payload
  -- Position packets must also contain the trailing on-ground Boolean.
  if LBS.null remaining then Nothing else Just (constructor positionX positionY positionZ)

decodeVarInt :: LBS.ByteString -> Maybe Int
decodeVarInt = go 0 0 . LBS.toStrict
 where
  go value shiftCount input
    | shiftCount >= 35 = Nothing
    | otherwise = do
        (byte, rest) <- BS.uncons input
        let value' = value .|. (fromIntegral (byte .&. 0x7f) `shiftL` shiftCount)
        if byte .&. 0x80 == 0
          then Just value'
          else go value' (shiftCount + 7) rest

decodeMinecraftString :: LBS.ByteString -> Maybe BS.ByteString
decodeMinecraftString input =
  either (const Nothing) (Just . fst) $
    decodeMinecraftStringBytes (LBS.toStrict input)

-- | Decode the handshaking packet's target state without exposing raw packet
-- framing concerns to the server coordinator.
parseHandshakeNextState :: BS.ByteString -> Either String Int
parseHandshakeNextState input = do
  (_, afterProtocol) <- decodeVarIntBytes input
  (_, afterAddress) <- decodeMinecraftStringBytes afterProtocol
  (_, afterPort) <- decodeWord16Bytes afterAddress
  fst <$> decodeVarIntBytes afterPort

parseLoginUsername :: BS.ByteString -> Either String BS.ByteString
parseLoginUsername = fmap fst . decodeMinecraftStringBytes

escapeJsonString :: BS.ByteString -> BS.ByteString
escapeJsonString = BS.concatMap escapeByte
 where
  escapeByte 8 = "\\b"
  escapeByte 9 = "\\t"
  escapeByte 10 = "\\n"
  escapeByte 12 = "\\f"
  escapeByte 13 = "\\r"
  escapeByte 34 = "\\\""
  escapeByte 92 = "\\\\"
  escapeByte byte
    | byte < 32 = BS.pack [92, 117, 48, 48, hexDigit (byte `shiftR` 4), hexDigit (byte .&. 15)]
  escapeByte byte = BS.singleton byte

  hexDigit nibble
    | nibble < 10 = 48 + nibble
    | otherwise = 87 + nibble

decodeMinecraftStringBytes :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
decodeMinecraftStringBytes input = do
  (length', remaining) <- decodeVarIntBytes input
  if length' < 0 || BS.length remaining < length'
    then Left "Invalid Minecraft string length"
    else Right (BS.take length' remaining, BS.drop length' remaining)

decodeVarIntBytes :: BS.ByteString -> Either String (Int, BS.ByteString)
decodeVarIntBytes = go 0 0
 where
  go value shiftCount input =
    case BS.uncons input of
      Nothing -> Left "Truncated VarInt"
      Just (byte, remaining)
        | shiftCount >= 35 -> Left "VarInt exceeds five bytes"
        | byte .&. 0x80 /= 0 -> go (value .|. (fromIntegral (byte .&. 0x7f) `shiftL` shiftCount)) (shiftCount + 7) remaining
        | otherwise -> Right (value .|. (fromIntegral byte `shiftL` shiftCount), remaining)

decodeWord16Bytes :: BS.ByteString -> Either String (Int, BS.ByteString)
decodeWord16Bytes input
  | BS.length input < 2 = Left "Truncated unsigned short"
  | otherwise = Right (fromIntegral (BS.index input 0) `shiftL` 8 .|. fromIntegral (BS.index input 1), BS.drop 2 input)

splitVarInt :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitVarInt = go BS.empty 0
 where
  go :: BS.ByteString -> Int -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
  go consumed count input
    | count >= 5 = Nothing
    | otherwise = do
        (byte, rest) <- BS.uncons input
        let consumed' = BS.snoc consumed byte
        if byte .&. 0x80 == 0
          then Just (consumed', rest)
          else go consumed' (count + 1) rest

-- | Read a VarInt.  A @Nothing@ result is a clean peer disconnect;
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

-- | Read a VarInt and return both the value and the number of bytes read.
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

-- | Build a VarInt from an Int. Minecraft VarInts are signed 32-bit values,
-- so we use Word32 for bitwise operations.
buildVarInt :: Int -> Builder
buildVarInt = go . fromIntegral
 where
  -- Minecraft VarInts are signed 32-bit values.  Encoding through Word32 is
  -- essential: arithmetic right shifts make a negative Int recurse forever.
  go :: Word32 -> Builder
  go value
    | value .&. complement 0x7F == 0 = word8 (fromIntegral value)
    | otherwise = word8 (fromIntegral ((value .&. 0x7F) .|. 0x80)) <> go (value `shiftR` 7)

-- Raw netcode

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

------------------------------------------------------

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
