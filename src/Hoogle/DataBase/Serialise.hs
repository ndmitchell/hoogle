
module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Data.Binary.Defer
import Data.Binary.Raw
import General.Base
import General.System

import Hoogle.DataBase.Type
import Data.Version


-- FIXME: Has become hard coded, go back to minor version lumps
hooVersion :: [Int]
hooVersion = [4,0,0,5] -- take 4 $ versionBranch version ++ repeat 0

hooString = "HOOG"


saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase file db = do
    h <- openBinaryFile file WriteMode
    mapM_ (hPutChar h) hooString
    mapM_ (hPutByte h) hooVersion
    runDeferPut h $ put db
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

    let showVer = showVersion . flip Version []
    ver <- replicateM 4 (hGetByte h)
    when (ver /= hooVersion) $
        error $ "Wrong hoogle database version: " ++ showVer ver ++
                " found, expected " ++ showVer hooVersion

    runDeferGet h get
