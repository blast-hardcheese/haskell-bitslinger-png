 {-# LANGUAGE OverloadedStrings #-}

module PngParser where

import Control.Applicative ((<$>), (*>), (<*>))
import Control.Monad ((>>), (>>=))

import Data.Bits (shift, (.|.))
import Data.Either
import Data.Word (Word8)

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

data PngChunk = Chunk {
    length :: Int,
    type' :: B.ByteString
} deriving Show

data PngStructure = Png {
    header :: B.ByteString,
    chunks :: [PngChunk]
} deriving Show

int32 :: Parser Int
int32 = (foldl (\a x -> (shift 8 a) .|. (fromIntegral x)) 0) <$> count 4 anyWord8

pngHeader = string $ B.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

pngChunk :: Parser PngChunk
pngChunk = Chunk <$> int32 <*> (B.pack <$> count 4 anyWord8)

pngFile :: Parser PngStructure
pngFile = Png <$> pngHeader <*> count 1 pngChunk

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
