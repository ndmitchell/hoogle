
module Hoogle.Operations.Convert(convert) where

import Hoogle.TextBase.All
import Hoogle.DataBase.All


-- error messages are given using trace and error
convert :: FilePath -> FilePath -> IO ()
convert tb db = do
    res <- parseTextBase tb
    case res of
        Left  x -> error $ show x
        Right x -> do
            let y = createDataBase x
            saveDataBase db y

            -- <debugging>
            z <- loadDataBase db
            print z
            if show y == show z then return () else
                error "Database did not match"
            -- </debugging>
