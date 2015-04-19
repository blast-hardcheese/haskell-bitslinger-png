module Png where

import Control.Applicative ((<$>), (*>), (<*>))
import Control.Monad ((>>), (>>=))

import Data.Bits (shift, xor, (.|.), (.&.))
import qualified Data.ByteString as B
import Data.Word (Word8, Word32)

import Debug.Trace (trace)

data ChunkData = ChunkIHDRData {
    width :: Word32,
    height :: Word32,
    bitDepth :: Word8,
    colorType :: Word8,
    compressionMethod :: Word8,
    filterMethod :: Word8,
    interlaceMethod :: Word8
} | ChunksBITData {
    red :: Word8,
    green :: Word8,
    blue :: Word8
} | ChunkpHYs {
    ppux :: Word32,
    ppuy :: Word32,
    unitSpecifier :: Word8
} | ChunktEXt {
    keyword :: B.ByteString,
    text :: B.ByteString
} | ChunkIDAT {
    bits :: B.ByteString
} | ChunkIEND {
} deriving Show

data PngChunk = Chunk {
    length :: Word32,
    type' :: B.ByteString,
    data' :: ChunkData,
    crc :: Word32
} deriving Show

data PngStructure = Png {
    header :: B.ByteString,
    chunks :: [PngChunk]
} deriving Show

buildCrc :: Word32 -> Word32
buildCrc c = foldr acc c [0..7]
    where acc _ a = if (a .&. 1) == 1 then
                        xor 0xedb88320 (shift a (-1))
                    else
                        shift a (-1)

crcTable :: [Word32]
crcTable = buildCrc <$> [0..255]

getCrc :: [Word8] -> Word32
getCrc bits = xor 0xffffffff $ foldl acc 0xffffffff bits
    where acc :: Word32 -> Word8 -> Word32
          acc a x = xor (crcTable !! (fromIntegral ((xor a $ fromIntegral x) .&. 0xff))) (shift a (-8))
