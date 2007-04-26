
module Hoogle.DataBase.All(
    module Hoogle.DataBase.Type,
    newDataBase
    ) where

import General.All
import Hoogle.DataBase.Type
import Hoogle.TextBase.All


newDataBase :: FilePath -> FilePath -> IO [Response]
newDataBase infile outfile = do
    x <- parseTextBase infile
    case x of
        Left x -> return [Error $ show x]
        Right x -> createDataBase x outfile
