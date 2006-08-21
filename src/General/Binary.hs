
module General.Binary where

import System.IO
import Control.Monad
import Data.Bits
import Data.Char


hPutInt :: Handle -> Int -> IO ()
hPutInt h = hPutStr h . map chr . map (0xff .&.)
                      . take 4 . iterate (`shiftR` 8)

hGetInt :: Handle -> IO Int
hGetInt h = replicateM 4 (hGetChar h) >>=
            return . foldr (\d i -> i `shiftL` 8 .|. ord d) 0



hPutIntText :: Handle -> Int -> IO ()
hPutIntText hndl n = hPutStr hndl ('#' : replicate (8-length s) '0' ++ s)
    where s = show n

hGetIntText :: Handle -> IO Int
hGetIntText hndl = do
    ('#':str) <- replicateM 9 $ hGetChar hndl
    return $ read str



hPutString :: Handle -> String -> IO ()
hPutString hndl str = do
    hPutInt hndl (length str)
    hPutStr hndl str

hGetString :: Handle -> IO String
hGetString hndl = do
    i <- hGetInt hndl
    replicateM i $ hGetChar hndl


hTellInt :: Handle -> IO Int
hTellInt = liftM fromInteger . hTell
