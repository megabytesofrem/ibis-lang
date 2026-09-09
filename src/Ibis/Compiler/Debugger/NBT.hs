module Ibis.Compiler.Debugger.NBT where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.Int (Int32, Int64)
import Data.Word (Word8)

data NBT
  = TagByte !Word8
  | TagInt !Int32
  | TagFloat !Float
  | TagString !BS.ByteString
  | TagList !Word8 [NBT]
  | TagCompound [(BS.ByteString, NBT)]
  | TagLongArray [Int64]

-- | Encode a named NBT root tag
buildRootNBT :: BS.ByteString -> NBT -> Builder
buildRootNBT name tag = word8 (tagTypeId tag) <> buildNBTString name <> buildNBTValue tag

tagTypeId :: NBT -> Word8
tagTypeId (TagByte _) = 1
tagTypeId (TagInt _) = 3
tagTypeId (TagFloat _) = 5
tagTypeId (TagString _) = 8
tagTypeId (TagList _ _) = 9
tagTypeId (TagCompound _) = 10
tagTypeId (TagLongArray _) = 12

buildNBTString :: BS.ByteString -> Builder
buildNBTString str = word16BE (fromIntegral $ BS.length str) <> byteString str

buildNBTValue :: NBT -> Builder
buildNBTValue (TagByte w) = word8 w
buildNBTValue (TagInt i) = int32BE i
buildNBTValue (TagFloat f) = floatBE f
buildNBTValue (TagString s) = buildNBTString s
buildNBTValue (TagList t xs) = word8 t <> int32BE (fromIntegral $ length xs) <> mconcat (map buildNBTValue xs)
buildNBTValue (TagLongArray lxs) = int32BE (fromIntegral $ length lxs) <> mconcat (map int64BE lxs)
buildNBTValue (TagCompound pairs) =
  mconcat [word8 (tagTypeId val) <> buildNBTString key <> buildNBTValue val | (key, val) <- pairs]
    <> word8 0x00 -- TAG_End