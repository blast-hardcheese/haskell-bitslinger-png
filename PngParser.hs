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
    _ <- (
        if typ /= "IDAT" then
            trace ("  found: " ++ (show dat)) $ return ()
        else
            trace ("  found IDAT") $ return ()
        )
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
decodeData "sRGB" _ _ = ChunksRGB <$> int8
decodeData "IDAT" len (Just ihdrChunk) = ChunkIDAT <$> AP.take len
decodeData "IEND" _ _ = return ChunkIEND
decodeData _ len _ = Chunk____ <$> AP.take len

dataParser :: ChunkData -> Parser IDATUnpacked
dataParser (ChunkIHDRData width height bitDepth colorType compressionMethod filterMethod interlaceMethod) = IDATUnpacked <$> (AP.count h rowParser)
    where w = fromIntegral $ width
          h = fromIntegral $ height
          pixelWidth = 3

          rowParser :: Parser IDATRow
          rowParser = IDATRow <$> int8 <*> (AP.count w pixelParser)

          pixelParser :: Parser IDATPixel
          pixelParser = TrueColorPixel <$> int8 <*> int8 <*> int8

parse :: B.ByteString -> Either String PngStructure
parse file = parseOnly (pngFile <* endOfInput) file

parsePixels :: ChunkData -> B.ByteString -> Either String IDATUnpacked
parsePixels header = handle . (AP.parse $ dataParser header)
    where handle (Fail _ _ e) = Left e
          handle (Partial _) = Left "Insufficient data"
          handle (Done _ r) = Right r
