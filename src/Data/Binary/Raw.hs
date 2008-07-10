
-- Note: Some of these functions, when written in a point-free
-- style have a significant impact on the runtime speed of
-- unoptimised code, for example hGetPos contributes ~20% extra time

module Data.Binary.Raw(
    hGetInt, hPutInt,
    hGetByte, hPutByte, maxByte,
    hGetPos, hSetPos
    ) where

import System.IO
import Control.Monad
import Data.Bits
import Data.Char


hGetPos :: Handle -> IO Integer
hGetPos hndl = hTell hndl

hSetPos :: Handle -> Integer -> IO ()
hSetPos hndl i = hSeek hndl AbsoluteSeek i


maxByte :: Int
maxByte = 0xff

-- FROM the Binary module, thanks to the Hac 07 people!

hPutInt :: Handle -> Int -> IO ()
hPutInt hndl w32 = do
    let w4 = (w32 `shiftR` 24) .&. 0xff
        w3 = (w32 `shiftR` 16) .&. 0xff
        w2 = (w32 `shiftR`  8) .&. 0xff
        w1 =  w32              .&. 0xff
    hPutByte hndl w1
    hPutByte hndl w2
    hPutByte hndl w3
    hPutByte hndl w4

hPutByte :: Handle -> Int -> IO ()
hPutByte hndl i = hPutChar hndl $ chr i


hGetInt :: Handle -> IO Int
hGetInt hndl = do
    w1 <- hGetByte hndl
    w2 <- hGetByte hndl
    w3 <- hGetByte hndl
    w4 <- hGetByte hndl
    return $! (w4 `shiftL` 24) .|.
              (w3 `shiftL` 16) .|.
              (w2 `shiftL`  8) .|.
              (w1)


hGetByte :: Handle -> IO Int
hGetByte hndl = liftM ord $ hGetChar hndl

