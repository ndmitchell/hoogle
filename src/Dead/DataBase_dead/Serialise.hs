
module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Data.Binary.Defer
import System.IO
import Control.Monad

import Hoogle.DataBase.Type


hooVersion = 1 :: Int
hooString = "HOOG"


saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase file db = do
    hndl <- openBinaryFile file WriteMode
    put hndl hooString
    put hndl hooVersion
    put hndl db
    hClose hndl


loadDataBase :: FilePath -> IO DataBase
loadDataBase file = do
    hndl <- openBinaryFile file ReadMode

    str <- get hndl
    when (str /= hooString) $
        error $ "Not a hoogle database: " ++ file

    ver <- get hndl
    when (ver /= hooVersion) $
        error $ "Wrong hoogle database version: " ++ show ver ++
                " found, expected " ++ show hooVersion

    get hndl
