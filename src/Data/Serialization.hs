{-# LANGUAGE ImportQualifiedPost #-}

module Data.Serialization where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getByteString, getInt32le, getWord32le, getWord64le, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putInt32le, putWord32le, putWord64le, runPut)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL

import Data.Int (Int32)
import Data.Word (Word32, Word64)

data SerializedChunk = SerializedChunk
  { chunkCoordX :: !Int32
  , chunkCoordY :: !Int32
  , chunkCoordZ :: !Int32
  , chunkDepth :: !Int32
  , chunkArrowIds :: ![Word64]
  , chunkSectionPayload :: !BS.ByteString
  }

-- Magic number for identifying serialized chunks
chunkMagicNumber :: BS.ByteString
chunkMagicNumber = BS.pack ['I', 'B', 'I', 'S']

putChunk :: SerializedChunk -> Put
putChunk (SerializedChunk x y z cdepth arrowIds payload) = do
  -- MAGIC
  putByteString chunkMagicNumber

  -- Chunk world coordinates (x, y, z)
  putInt32le x
  putInt32le y
  putInt32le z

  -- Depth of the covering arrow
  putInt32le cdepth

  -- Covering arrow array header and payloads (length + bytes)
  putWord32le (fromIntegral $ length arrowIds)
  mapM_ putWord64le arrowIds

  -- Presheaf section data (length + bytes)
  putWord32le (fromIntegral $ BS.length payload)
  putByteString payload

getChunk :: Get SerializedChunk
getChunk = do
  magic <- getByteString 4
  if magic /= chunkMagicNumber
    then fail "Invalid chunk magic number"
    else do
      -- Chunk world coordinates (x, y, z)
      x <- getInt32le
      y <- getInt32le
      z <- getInt32le

      -- Depth of the covering arrow
      cdepth <- getInt32le

      -- Covering arrow array header and payloads (length + bytes)
      arrowCount <- fromIntegral <$> getWord32le
      arrowIds <- replicateM arrowCount getWord64le

      -- Presheaf section data (length + bytes)
      payloadLength <- fromIntegral <$> getWord32le
      payload <- getByteString payloadLength

      pure $ SerializedChunk x y z cdepth arrowIds payload

serializeChunk :: SerializedChunk -> BS.ByteString
serializeChunk = BSL.toStrict . runPut . putChunk

deserializeChunk :: BS.ByteString -> Either String SerializedChunk
deserializeChunk bs = case runGetOrFail getChunk (BSL.fromStrict bs) of
  Left (_, _, err) -> Left err
  Right (_, _, chunk) -> Right chunk