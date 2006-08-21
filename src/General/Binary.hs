
module General.Binary where

import System.IO
import Control.Monad


-- > hPutInt h = hPutStr h . map chr . map (0xff .&.)
-- >                       . take 4 . iterate (`shiftR` 8)
-- >
-- > hGetInt h = replicateM 4 (hGetChar h) >>=
-- >             return . foldr (\i d -> i `shiftL` 8 .|. ord d) 0

hPutInt :: Handle -> Int -> IO ()
hPutInt hndl n = hPutStr hndl ('#' : replicate (8-length s) '0' ++ s)
    where s = show n


hGetInt :: Handle -> IO Int
hGetInt hndl = do
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
