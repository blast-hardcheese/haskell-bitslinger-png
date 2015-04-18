 {-# LANGUAGE OverloadedStrings #-}

module PngParser where

import Control.Applicative ((<$>), (*>), (<*>))
import Control.Monad ((>>), (>>=))

import Data.Bits (shift, (.|.))
import Data.Either
import Data.Word (Word8)

import Debug.Trace (trace)

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP

data ChunkData = ChunkIHDRData {
    width :: Int,
    height :: Int,
    bitDepth :: Int,
    colorType :: Int,
    compressionMethod :: Int,
    filterMethod :: Int,
    interlaceMethod :: Int
} | ChunksBITData {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving Show

data PngChunk = Chunk {
    length :: Int,
    type' :: B.ByteString,
    data' :: ChunkData,
    crc :: Int
} deriving Show

data PngStructure = Png {
    header :: B.ByteString,
    chunks :: [PngChunk]
} deriving Show

int8 :: Parser Int
int8 = fromIntegral <$> anyWord8

int32 :: Parser Int
int32 = (foldl (\a x -> (shift 8 a) .|. (fromIntegral x)) 0) <$> count 4 anyWord8

pngHeader = string $ B.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

pngChunk :: Parser PngChunk
pngChunk = do
    len <- int32
    typ <- AP.take 4
    _ <- trace ("Taking " ++ (show len) ++ " bytes of " ++ (show typ)) $ return ()
    (bits, dat) <- match $ decodeData typ
    _ <- trace ("  found: " ++ (show dat)) $ return ()
    crc <- int32
    _ <- trace ("  CRC: " ++ (show crc)) $ return ()
    return $ Chunk len typ dat crc

pngFile :: Parser PngStructure
pngFile = Png <$> pngHeader <*> count 1 pngChunk

decodeData :: B.ByteString -> Parser ChunkData
decodeData "IHDR" = ChunkIHDRData <$> int32 <*> int32 <*> int8 <*> int8 <*> int8 <*> int8 <*> int8
decodeData "sBIT" = ChunksBITData <$> int8 <*> int8 <*> int8 -- Incomplete implementation. Hardcoded for truecolor images.

insertIndex :: Int -> B.ByteString -> B.ByteString -> B.ByteString
insertIndex idx toInsert file = B.concat [B.take idx file, toInsert, B.drop idx file]

dropIndex :: Int -> B.ByteString -> B.ByteString
dropIndex idx = dropRange idx 1

dropRange :: Int -> Int -> B.ByteString -> B.ByteString
dropRange idx range file = B.append (B.take idx file) (B.drop (idx + range) file)

fix :: B.ByteString -> IO B.ByteString
fix file = do
    let first = insertIndex 4 (B.pack [0x0d]) file
    putStrLn $ show $ B.take 10 $ B.drop 8 first
    return first

parse :: B.ByteString -> IO (Either String String)
parse file = do
    parseTest pngFile file
    return $ Left "Not implemented"
