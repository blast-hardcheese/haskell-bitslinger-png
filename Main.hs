module Main where

import qualified Data.ByteString as B
import Control.Applicative ((<$>))

import PngParser (parse, fix)
import Png

recrc :: PngChunk -> PngChunk
recrc chunk = chunk { crc = getCrc $ B.unpack $ B.append (type' chunk) (encodeData $ data' chunk) }

main :: IO ()
main = do
    fixed <- (B.readFile "corrupt_735acee15fa4f3be8ecd0c6bcf294fd4.png" >>= fix)
    let (Right res) = parse fixed
    putStrLn "Seems to have parsed"
    let res2 = res { chunks = recrc <$> (chunks res) }
    B.writeFile "roundtrip.png" $ encodePng res2
