
module CmdLine.Load(loadQueryDatabases, availableDatabases) where

import Hoogle
import General.Base
import General.Util
import General.System
import System.FilePath


-- | Given a list of search directories, and a query, load the databases you
--   need, and return a list of those that you couldn't find
loadQueryDatabases :: [FilePath] -> Query -> IO ([String],Database)
loadQueryDatabases paths q = do
    let findFile = findM doesFileExist
    let xs = queryDatabases q
    fmap (second mconcat . partitionEithers) $ forM xs $ \x -> do
        r <- findFile [p </> x <.> "hoo" | p <- paths]
        case r of
            Nothing -> return $ Left x
            Just x -> fmap Right $ loadDatabase x


availableDatabases :: [FilePath] -> IO [String]
availableDatabases xs = fmap (sortBy compareString . nub . concat) $ forM xs $ \x -> do
    b <- doesDirectoryExist x
    ys <- if b then getDirectoryContents x else return []
    return [dropExtension y | y <- ys, takeExtension y == ".hoo"] 
