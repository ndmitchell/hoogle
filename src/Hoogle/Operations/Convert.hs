
module Hoogle.Operations.Convert(convert) where

import Hoogle.TextBase.All
import Hoogle.DataBase.All


-- error messages are given using trace and error
convert :: FilePath -> FilePath -> IO ()
convert tb db = do
    res <- parseTextBase tb
    case res of
        Left  x -> error $ show x
        Right x -> saveDataBase db (createDataBase x)
