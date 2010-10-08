
module Console.Files(getDataBaseFiles) where

import CmdLine.All
import CmdLine.Find
import General.Code
import Hoogle.Query.All
import Paths_hoogle(getDataDir)


-- pick "default" if there are not ones specified
-- otherwise use the CmdFlag and any +package query flags
getDataBaseFiles :: CmdLine -> Query -> IO [FilePath]
getDataBaseFiles flags q = do
    xs <- getDataBaseFilesNoDefault flags q
    if null xs
        then liftM (:[]) $ resolve flags "default"
        else return xs


getDataBaseFilesNoDefault :: CmdLine -> Query -> IO [FilePath]
getDataBaseFilesNoDefault flags q = do
    let plusPkg = [x | PlusPackage x <- scope q]
    rs <- mapM (resolve flags) plusPkg
    return rs


resolve :: CmdLine -> String -> IO FilePath
resolve flags x = do
    let inc = databaseDir flags
    dataDir <- getDataDir
    [x] <- findFile (inc++[dataDir]) ["hoo"] x
    return x
