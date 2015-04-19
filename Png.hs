module Png where

import Control.Applicative ((<$>), (*>), (<*>))
import Control.Monad ((>>), (>>=))

import Data.Bits (shift, xor, (.|.), (.&.))

import Data.Word (Word8, Word32)

import Debug.Trace (trace)

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
