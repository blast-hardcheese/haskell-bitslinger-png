{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<$>))

import Debug.Trace (trace)

import Data.Word (Word8, Word32)
import Data.Maybe (fromMaybe)
import Data.Either

import PngParser (parse, parsePixels)
import Png

import qualified Codec.Compression.Zlib as Z

recrc :: PngChunk -> PngChunk
recrc chunk = chunk { crc = getCrc $ B.unpack $ B.append (type' chunk) (encodeData $ data' chunk) }

relen :: PngChunk -> PngChunk
relen chunk = chunk { Png.length = fromIntegral $ B.length (encodeData $ data' chunk) }

shiftReplace :: [Int] -> B.ByteString -> B.ByteString
shiftReplace offsets bits = unix2dos offsets $ B.take (B.length bits - Prelude.length offsets) bits

unix2dos :: [Int] -> B.ByteString -> B.ByteString
unix2dos offsets bits = foldl (\bits x -> repl x bits) bits $ (uncurry (+)) <$> (zip [0..] offsets)
    where find' :: Int -> B.ByteString -> (Maybe Int)
          find' n bits = B.findIndex (== 10) (B.drop n bits)
          repl n bits = B.concat [B.take n bits, (B.pack [13]), B.drop n bits]

emblacken :: B.ByteString -> B.ByteString
emblacken bits = trace ("similar? " ++ (show $ B.length bits) ++ " ++ " ++ (show $ B.length o)) o
    where o = B.pack $ take (B.length bits) $ repeat 0

alter :: Int -> Int -> PngChunk -> PngChunk
-- alter _ 0 chunk | type' chunk == "IHDR" = chunk { data' = (data' chunk) { width = 960, height = 480 } }
-- alter _ 4 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = emblacken $ bits $ data' chunk } }
-- alter _ 4 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (shiftReplace [549] $ bits $ data' chunk) } }
alter _ 5 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (shiftReplace [498, 26270, 125127] $ bits $ data' chunk) } }
alter _ 6 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (shiftReplace [71422] $ bits $ data' chunk) } }
alter z 7 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (shiftReplace [z] $ bits $ data' chunk) } }
-- alter z 11 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (B.pack $ Prelude.concat [take (B.length $ bits $ data' chunk) (repeat 0), take z $ repeat 0]) } }
-- alter z 11 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (shiftReplace (take z $ repeat 0) $ bits $ data' chunk) } }
-- alter z 7 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = B.pack (Prelude.take z $ repeat 0) } }
-- alter z 8 chunk | type' chunk == "IDAT" = chunk { data' = (data' chunk) { bits = (shiftReplace [z] $ bits $ data' chunk) } }
-- 8
-- 9
-- 10
-- 12
alter _ _ chunk = chunk

iteralter :: PngStructure -> Int -> IO ()
iteralter res z = do
    putStrLn $ "iteralter: " ++ show z
    let res2 = res { chunks = (relen <$> recrc <$> (uncurry (alter z)) <$> (zip [0..] (chunks res))) }
    B.writeFile ("offsets/offsets-" ++ (show z) ++ ".png") $ encodePng res2

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
                insertIndex (sum $ Prelude.take 13 sizes) (B.pack [0x00]),
                replaceRange 0 0 (B.pack [])
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

          insertIndex :: Int -> B.ByteString -> B.ByteString -> B.ByteString
          insertIndex idx toInsert file = B.concat [B.take idx file, toInsert, B.drop idx file]

          dropIndex :: Int -> B.ByteString -> B.ByteString
          dropIndex idx = dropRange idx 1

          dropRange :: Int -> Int -> B.ByteString -> B.ByteString
          dropRange idx range file = B.append (B.take idx file) (B.drop (idx + range) file)

          replaceRange :: Int -> Int -> B.ByteString -> B.ByteString -> B.ByteString
          replaceRange idx len repl file = insertIndex idx repl $ dropRange idx len file


main :: IO ()
main = do
    -- fixed <- (B.readFile "corrupt_735acee15fa4f3be8ecd0c6bcf294fd4.png" >>= fix)
    fixed <- B.readFile "samples/white-2x1.png"
    let res = (parse fixed)
    putStrLn $ if isLeft res then
            show res
        else
            seq res "Seems to have parsed"

    let (Right img) = res
    let z = bits $ data' $ (chunks img) !! 4
    let ihdr = (chunks img) !! 0

    let dat = extractDecompressedData img
    let (Right parsed) = parsePixels (data' ihdr) $ dat
    let newChunks = relen <$> recrc <$> (chunkImageData 255 $ encodePixelsCompressed parsed)
    putStrLn $ "Decompressed! " ++ (show dat)
    putStrLn $ "Decompressed! " ++ (show parsed)
    putStrLn $ "Chunked: " ++ (show newChunks)

    let png = encodePng $ Png (B.pack [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]) $ (relen <$> recrc <$> [
                    Chunk 0 "IHDR" (ChunkIHDRData 1 2 8 2 0 0 0) 0,
                    Chunk 0 "pHYs" (ChunkpHYs 2835 2835 1) 0
                ]) ++ newChunks

    _ <- B.writeFile "/tmp/out.png" png

    let res = parse png
    putStrLn $ if isLeft res then
            show res
        else
            seq res "Seems to have parsed"

    let (Right img) = res

    let dat = extractDecompressedData img
    let (Right parsed) = parsePixels (data' ihdr) $ dat
    let newChunks = relen <$> recrc <$> (chunkImageData 255 $ encodePixelsCompressed parsed)
    putStrLn $ "Decompressed! " ++ (show dat)
    putStrLn $ "Decompressed! " ++ (show parsed)
    putStrLn $ "Chunked: " ++ (show newChunks)


    putStrLn "Done"
