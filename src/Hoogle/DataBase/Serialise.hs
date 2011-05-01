{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Hoogle.Store.All
import General.Base
import General.System

import Hoogle.DataBase.Type
import Paths_hoogle(version)
import Data.Version


hooVersion = take 4 $ map fromIntegral (versionBranch version) ++ [0..]
hooString = "HOOG"

data Identity = Identity deriving (Show, Typeable)

instance Store Identity where
    put Identity = mapM_ put hooString >> mapM_ putByte hooVersion
    get = do
        cs <- replicateM 4 get
        vr <- replicateM 4 getByte
        when (cs /= hooString) $
            error $ "Not a hoogle database"

        let showVer = intercalate "." . map show
        when (vr /= hooVersion) $
            error $ "Wrong hoogle database version: found " ++ showVer vr ++ ", " ++
                    "expected " ++ showVer hooVersion
        return Identity



saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase file db = runSPut file $ put (Identity, db)


loadDataBase :: FilePath -> IO DataBase
loadDataBase file = do
    sz <- withFile file ReadMode hFileSize
    when (sz < 12) $
        error $ "Not a hoogle database: " ++ file

    (Identity,db) <- runSGet file get
    return db
