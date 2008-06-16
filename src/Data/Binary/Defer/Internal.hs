
module Data.Binary.Defer.Internal(
    hGetInt, hPutInt,
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
    putWord8 hndl w1
    putWord8 hndl w2
    putWord8 hndl w3
    putWord8 hndl w4

putWord8 :: Handle -> Int -> IO ()
putWord8 hndl = hPutChar hndl . chr


hGetInt :: Handle -> IO Int
hGetInt hndl = do
    w1 <- getWord8 hndl
    w2 <- getWord8 hndl
    w3 <- getWord8 hndl
    w4 <- getWord8 hndl
    return $! (w4 `shiftL` 24) .|.
              (w3 `shiftL` 16) .|.
              (w2 `shiftL`  8) .|.
              (w1)


getWord8 :: Handle -> IO Int
getWord8 hndl = hGetChar hndl >>= return . ord

