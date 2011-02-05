
module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Hoogle.Store.All
import General.Base
import General.System

import Hoogle.DataBase.Type


-- FIXME: Has become hard coded, go back to minor version lumps
hooVersion = [4,0,0,5]
hooString = "HOOG"

data Identity = Identity

instance BinaryDefer Identity where
    put Identity = mapM_ putChr hooString >> mapM_ putByte hooVersion
    get = do
        cs <- replicateM 4 getChr
        vr <- replicateM 4 getByte
        when (cs /= hooString) $
            error $ "Not a hoogle database"

        let showVer = intercalate "." . map show
        when (vr /= hooVersion) $
            error $ "Wrong hoogle database version: " ++ showVer vr ++
                    " found, expected " ++ showVer hooVersion
        return Identity



saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase file db = do
    h <- openBinaryFile file WriteMode
    runDeferPut h $ put (Identity, db)
    hClose h


loadDataBase :: FilePath -> IO DataBase
loadDataBase file = do
    sz <- withFile file ReadMode hFileSize
    when (sz < 12) $
        error $ "Not a hoogle database: " ++ file

    (Identity,db) <- runDeferGet file get
    return db
