
module Hoogle.Operations.Combine(combine) where

import General.Code
import Hoogle.DataBase.All


combine :: [FilePath] -> FilePath -> IO ()
combine infiles outfile = do
    dbs <- mapM loadDataBase infiles
    saveDataBase outfile $ combineDataBase dbs
