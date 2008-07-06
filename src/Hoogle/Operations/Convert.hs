
module Hoogle.Operations.Convert(convert) where

import General.Code
import Hoogle.TextBase.All
import Hoogle.DataBase.All


-- error messages are given using trace and error
convert :: Bool -> FilePath -> FilePath -> IO ()
convert debugCheck tb db = do
    res <- parseTextBase tb
    case res of
        Left  x -> error $ show x
        Right x -> do
            let y = createDataBase x
            saveDataBase db y

            when (debugCheck) $ do
                putStr "Validating creation: "
                hFlush stdout
                z <- loadDataBase db
                if show y == show z
                    then putStrLn "Success"
                    else exitMessage ["Failure, database did not match"]

