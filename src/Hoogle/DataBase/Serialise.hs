
module Hoogle.DataBase.Serialise(
    saveDataBase, loadDataBase
    ) where

import Data.Binary.Defer
import Data.Binary.Raw
import General.Code

import Hoogle.DataBase.Type
import Paths_hoogle
import Data.Version


hooVersion :: [Int]
hooVersion = take 4 $ versionBranch version ++ repeat 0

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
    when (ver /= hooVersion && "hugs" `notElem` versionTags version) $
        error $ "Wrong hoogle database version: " ++ showVer ver ++
                " found, expected " ++ showVer hooVersion

    runDeferGet h get
