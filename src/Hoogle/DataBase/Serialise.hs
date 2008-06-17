
module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Data.Binary.Defer
import Data.Binary.Raw
import System.IO
import Control.Monad

import Hoogle.DataBase.Type


hooVersion = 1 :: Int
hooString = "HOOG"


saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase file db = do
    h <- openBinaryFile file WriteMode
    mapM_ (hPutChar h) hooString
    hPutInt h hooVersion
    runDeferPut h (put db)
    hClose h


loadDataBase :: FilePath -> IO DataBase
loadDataBase file = do
    h <- openBinaryFile file ReadMode
    size <- hFileSize h

    when (size < 8) $
        error $ "Not a hoogle database: " ++ file

    str <- replicateM 4 (hGetChar h)
    when (str /= hooString) $
        error $ "Not a hoogle database: " ++ file

    ver <- hGetInt h
    when (ver /= hooVersion) $
        error $ "Wrong hoogle database version: " ++ show ver ++
                " found, expected " ++ show hooVersion

    runDeferGet h get
