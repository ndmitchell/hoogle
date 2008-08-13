
module CmdLine.Files(getDataBaseFiles, getDataBaseFilesNoDefault) where

import CmdLine.Flag
import General.Glob
import General.Code
import Hoogle.Query.All
import Paths_hoogle(getDataDir)


-- pick "default" if there are not ones specified
-- otherwise use the CmdFlag and any +package query flags
getDataBaseFiles :: [CmdFlag] -> Query -> IO [FilePath]
getDataBaseFiles flags q = do
    xs <- getDataBaseFilesNoDefault flags q
    if null xs
        then liftM (:[]) $ resolve flags "default"
        else return xs


getDataBaseFilesNoDefault :: [CmdFlag] -> Query -> IO [FilePath]
getDataBaseFilesNoDefault flags q = do
    let dataFil = [x | DataFile x <- flags]
        plusPkg = [x | PlusPackage x <- scope q]
    rs <- mapM (resolve flags) plusPkg
    return $ dataFil ++ rs


resolve :: [CmdFlag] -> String -> IO FilePath
resolve flags x = do
    let inc = [x | Include x <- flags]
    dataDir <- getDataDir
    [x] <- globFile (inc++[dataDir]) ["hoo"] x
    return x
