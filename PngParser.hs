{-# LANGUAGE OverloadedStrings #-}

module PngParser where

import Control.Applicative ((<$>), (*>), (<*>), (<*))
import Control.Monad ((>>), (>>=))

import Data.Bits (shift, (.|.))
import Data.Either
import Data.Word (Word8, Word32)

import Debug.Trace (trace)

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP

import Png

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

int8 :: Parser Word8
int8 = fromIntegral <$> anyWord8

int32 :: Parser Word32
int32 = (foldl (\a x -> (shift a 8) .|. (fromIntegral x)) 0) <$> count 4 anyWord8

pngHeader = string $ B.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

pngChunk :: Maybe PngChunk -> Parser PngChunk
pngChunk maybeIHDR = do
    len <- int32
    typ <- AP.take 4
    _ <- trace ("Taking " ++ (show len) ++ " bytes of " ++ (show typ)) $ return ()
    (bits, dat) <- match $ decodeData typ (fromIntegral len) maybeIHDR
    _ <- trace ("  found: " ++ (show dat)) $ return ()
    crc <- int32
    let calcCRC = getCrc $ B.unpack (B.append typ bits)
    _ <- trace ("  CRC valid: " ++ (show $ calcCRC == crc) ++ " (" ++ (show crc) ++ " == " ++ (show calcCRC) ++ ")") $ return ()
    return $ Chunk len typ dat crc

pngFile :: Parser PngStructure
pngFile = do
    header <- pngHeader
    ihdr <- pngChunk Nothing
    chunks <- (many' $ pngChunk (Just ihdr))
    _ <- endOfInput
    return $ Png header (ihdr : chunks)

decodeData :: B.ByteString -> Int -> Maybe PngChunk -> Parser ChunkData
decodeData "IHDR" _ _ = ChunkIHDRData <$> int32 <*> int32 <*> int8 <*> int8 <*> int8 <*> int8 <*> int8
decodeData "sBIT" _ _ = ChunksBITData <$> int8 <*> int8 <*> int8 -- Incomplete implementation. Hardcoded for truecolor images.
decodeData "pHYs" _ _ = ChunkpHYs <$> int32 <*> int32 <*> int8
decodeData "tEXt" len _ = do
    kw <- AP.takeWhile (/= 0x00)
    _ <- word8 0x00
    let left = len - (B.length kw) - 1
    text <- AP.take left
    return $ ChunktEXt kw text
decodeData "IDAT" len (Just ihdrChunk) = ChunkIDAT <$> AP.take len
decodeData "IEND" _ _ = return ChunkIEND

insertIndex :: Int -> B.ByteString -> B.ByteString -> B.ByteString
insertIndex idx toInsert file = B.concat [B.take idx file, toInsert, B.drop idx file]

dropIndex :: Int -> B.ByteString -> B.ByteString
dropIndex idx = dropRange idx 1

dropRange :: Int -> Int -> B.ByteString -> B.ByteString
dropRange idx range file = B.append (B.take idx file) (B.drop (idx + range) file)

fix :: B.ByteString -> IO B.ByteString
fix file = do
    let res = foldl (\ a f -> f a) file [
                insertIndex 4 (B.pack [0x0d]),
                insertIndex (sum $ Prelude.take 5 sizes) (B.pack [0x00]),
                insertIndex (sum $ Prelude.take 6 sizes) (B.pack [0x00, 0x00, 0x00]),
                insertIndex (sum $ Prelude.take 7 sizes) (B.pack [0x00]),
                insertIndex (sum $ Prelude.take 9 sizes) (B.pack [0x00, 0x00, 0x00]),
                insertIndex (sum $ Prelude.take 10 sizes) (B.pack [0x00]),
                insertIndex (sum $ Prelude.take 11 sizes) (B.pack [0x00, 0x00]),
                insertIndex (sum $ Prelude.take 13 sizes) (B.pack [0x00])
            ]

    B.writeFile "fixed.png" res
    return res
    where sizes = [
                      4 * 3 + 13,
                      4 * 3 + 3,
                      4 * 3 + 9,
                      4 * 3 + 28,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 131072,
                      4 * 3 + 8559,
                      4 * 3 + 0
                  ]

parse :: B.ByteString -> Either String PngStructure
parse file = parseOnly (pngFile <* endOfInput) file
