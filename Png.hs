{-# LANGUAGE OverloadedStrings #-}

module Png where

import Control.Applicative ((<$>), (*>), (<*>))
import Control.Monad ((>>), (>>=))

import Data.Bits (shift, xor, (.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)

import qualified Codec.Compression.Zlib as Z

import Debug.Trace (trace)

data IDATPixel = TrueColorPixel Word8 Word8 Word8
    deriving Show

data IDATRow = IDATRow {
    filterType :: Word8,
    pixels :: [IDATPixel]
} deriving Show

data IDATUnpacked = IDATUnpacked {
    rows :: [IDATRow]
} deriving Show

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
} | ChunksRGB {
    renderingIntent :: Word8
} | Chunk____ {
    bits :: B.ByteString
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

unint8 :: Word8 -> B.ByteString
unint8 n = B.pack [n]

unint32 :: Word32 -> B.ByteString
unint32 n = B.pack $ foldl (\a x -> (fromIntegral $ (shift n (-x * 8)) .&. 0xff) : a) [] [0..3]

encodeData :: ChunkData -> B.ByteString
encodeData (ChunkIHDRData width height bitDepth colorType compressionMethod filterMethod interlaceMethod) = B.concat [unint32 width, unint32 height, unint8 bitDepth, unint8 colorType, unint8 compressionMethod, unint8 filterMethod, unint8 interlaceMethod]
encodeData (ChunksBITData red green blue) = B.concat [unint8 red, unint8 green, unint8 blue]
encodeData (ChunkpHYs ppux ppuy unitSpecifier) = B.concat [unint32 ppux, unint32 ppuy, unint8 unitSpecifier]
encodeData (ChunktEXt keyword text) = B.concat [keyword, unint8 0x00, text]
encodeData (ChunkIDAT bits) = bits
encodeData ChunkIEND = B.concat []

encodeChunk :: PngChunk -> B.ByteString
encodeChunk (Chunk length type' data' crc) = B.concat [unint32 length, type', encodeData data', unint32 crc]

encodePng :: PngStructure -> B.ByteString
encodePng (Png header chunks) = B.append header $ B.concat $ encodeChunk <$> chunks

extractData :: PngStructure -> B.ByteString
extractData (Png _ chunks) = B.concat $ bits <$> data' <$> idats
    where idats = Prelude.filter ((== "IDAT") . type') chunks

extractDecompressedData :: PngStructure -> B.ByteString
extractDecompressedData = BL.toStrict . Z.decompress . BL.fromStrict . extractData

encodePixels :: IDATUnpacked -> B.ByteString
encodePixels (IDATUnpacked rows) = B.concat $ encodeRow <$> rows
    where encodeRow (IDATRow filterType pixels) = B.append (unint8 filterType) $ B.concat (encodePixels <$> pixels)
          encodePixels :: IDATPixel -> B.ByteString
          encodePixels (TrueColorPixel red green blue) = B.concat [unint8 red, unint8 green, unint8 blue]

encodePixelsCompressed :: IDATUnpacked -> B.ByteString
encodePixelsCompressed = BL.toStrict . Z.compress . BL.fromStrict . encodePixels

chunkImageData :: Int -> B.ByteString -> [PngChunk]
chunkImageData chunkSize dat = work $ B.splitAt chunkSize dat
    where toChunk dat = Chunk 0 "IDAT" (ChunkIDAT dat) 0
          work :: (B.ByteString, B.ByteString) -> [PngChunk]
          work (first, "") = [toChunk first]
          work (first, rest) = (toChunk first) : (chunkImageData chunkSize rest)
