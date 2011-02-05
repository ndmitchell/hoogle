
module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Hoogle.Store.All
import General.Base
import General.System

import Hoogle.DataBase.Type


-- FIXME: Has become hard coded, go back to minor version lumps
hooVersion :: (Word8,Word8,Word8,Word8)
hooVersion = (4,0,0,5)

hooString = "HOOG"


data DB = DB String (Word8,Word8,Word8,Word8) (Defer DataBase)

instance BinaryDefer DB where
    put (DB x1 x2 x3) = put3 x1 x2 x3
    get = get3 DB


saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase file db = do
    h <- openBinaryFile file WriteMode
    runDeferPut h $ put $ DB hooString hooVersion $ Defer db
    hClose h


loadDataBase :: FilePath -> IO DataBase
loadDataBase file = do
    sz <- withFile file ReadMode hFileSize
    when (sz < 12) $
        error $ "Not a hoogle database: " ++ file

    h <- openBinaryFile file ReadMode
    DB str ver db <- runDeferGet h get

    when (str /= hooString) $
        error $ "Not a hoogle database: " ++ file

    when (ver /= hooVersion) $
        error $ "Wrong hoogle database version: " ++ show ver ++
                " found, expected " ++ show hooVersion

    return $ fromDefer db
