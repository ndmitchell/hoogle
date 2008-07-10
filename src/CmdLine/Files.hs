
module CmdLine.Files(getDataBaseFiles) where

import CmdLine.Flag
import General.Glob
import General.Code
import Hoogle.Query.All


-- pick "default" if there are not ones specified
-- otherwise use the CmdFlag and any +package query flags
getDataBaseFiles :: [CmdFlag] -> Query -> IO [FilePath]
getDataBaseFiles flags q = do
    let dataFil = [x | DataFile x <- flags]
        plusPkg = [x | PlusPackage x <- scope q]
    if null dataFil && null plusPkg then
        liftM (:[]) $ resolve "default"
     else do
        rs <- mapM resolve plusPkg
        return $ dataFil ++ rs
    where
        inc = [x | Include x <- flags]
        resolve x = do
            [x] <- globFile inc ["hoo"] x
            return x
