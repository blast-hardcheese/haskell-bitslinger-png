module Main where

import qualified Data.ByteString as B
import Control.Applicative ((<$>))

import PngParser (parse, fix)

main :: IO ()
main = do
    result <- (B.readFile "corrupt_735acee15fa4f3be8ecd0c6bcf294fd4.png" >>= fix >>= parse)
    putStrLn "hey"
