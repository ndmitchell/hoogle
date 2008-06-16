
module Data.Binary.Raw(
    hGetInt, hPutInt,
    hGetByte, hPutByte,
    hGetPos, hSetPos
    ) where

import System.IO
import Control.Monad
import Data.Bits
import Data.Char


hGetPos :: Handle -> IO Int
hGetPos = liftM fromInteger . hTell

hSetPos :: Handle -> Int -> IO ()
hSetPos hndl = hSeek hndl AbsoluteSeek . toInteger


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
hPutByte hndl = hPutChar hndl . chr


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
hGetByte hndl = hGetChar hndl >>= return . ord

