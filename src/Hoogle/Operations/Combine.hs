
module Hoogle.Operations.Combine(combine) where

import Hoogle.DataBase.All


combine :: [FilePath] -> FilePath -> IO ()
combine infiles outfile = do
    dbs <- mapM loadDataBase infiles
    saveDataBase outfile $ combineDataBase dbs
