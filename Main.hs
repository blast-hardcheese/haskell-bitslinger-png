module Main where

import qualified Data.ByteString as B
import Control.Applicative ((<$>))

import PngParser (parse, fix)

main :: IO ()
main = do
    fixed <- (B.readFile "corrupt_735acee15fa4f3be8ecd0c6bcf294fd4.png" >>= fix)
    let res = parse fixed
    putStrLn $ show res
