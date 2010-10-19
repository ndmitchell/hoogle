
module Hoogle.Operations.DataBases(loadDataBases) where

import Hoogle.DataBase.All
import Hoogle.Query.All
import System.Directory
import System.FilePath
import Control.Monad
import Data.Either


-- | Given a list of search directories, and a query, load the databases you
--   need, and return a list of those that you couldn't find
loadDataBases :: [FilePath] -> Query -> IO ([String],[DataBase])
loadDataBases paths q = do
    let xs = [x | PlusPackage x <- scope q]
    fmap partitionEithers $ forM (if null xs then ["default"] else xs) $ \x -> do
        r <- findFile [p </> x <.> "hoo" | p <- paths]
        case r of
            Nothing -> return $ Left x
            Just x -> fmap Right $ loadDataBase x
    

findFile :: [FilePath] -> IO (Maybe FilePath)
findFile [] = return Nothing
findFile (x:xs) = do
    b <- doesFileExist x
    if b then return $ Just x else findFile xs
