{-# LANGUAGE OverloadedStrings #-}

module PngParser where

import Control.Applicative ((<$>), (*>), (<*>))
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
    _ <- trace ("  CRC: " ++ (show crc)) $ return ()
    _ <- trace ("    isValid: " ++ (show $ (getCrc $ B.unpack (B.append typ bits)) == crc)) $ return ()
    return $ Chunk len typ dat crc

pngFile :: Parser PngStructure
pngFile = do
    header <- pngHeader
    ihdr <- pngChunk Nothing
    chunks <- (many' $ pngChunk (Just ihdr))
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

insertIndex :: Int -> B.ByteString -> B.ByteString -> B.ByteString
insertIndex idx toInsert file = B.concat [B.take idx file, toInsert, B.drop idx file]

dropIndex :: Int -> B.ByteString -> B.ByteString
dropIndex idx = dropRange idx 1

dropRange :: Int -> Int -> B.ByteString -> B.ByteString
dropRange idx range file = B.append (B.take idx file) (B.drop (idx + range) file)

fix :: B.ByteString -> IO B.ByteString
fix file = do
    let first = insertIndex 4 (B.pack [0x0d]) file
    let second = first
    B.writeFile "fixed.png" second
    return second

parse :: B.ByteString -> IO (Either String String)
parse file = do
    parseTest pngFile file
    return $ Left "Not implemented"
