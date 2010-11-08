
module CmdLine.Load(loadQueryDatabases) where

import Control.Arrow
import Control.Monad
import Data.Either
import Data.Monoid
import Hoogle
import System.Directory
import System.FilePath


-- | Given a list of search directories, and a query, load the databases you
--   need, and return a list of those that you couldn't find
loadQueryDatabases :: [FilePath] -> Query -> IO ([String],Database)
loadQueryDatabases paths q = do
    let xs = queryDatabases q
    fmap (second mconcat . partitionEithers) $ forM xs $ \x -> do
        r <- findFile [p </> x <.> "hoo" | p <- paths]
        case r of
            Nothing -> do
                r <- findFile [p </> x <.> "txt" | p <- paths]
                case r of
                    Nothing -> return $ Left x
                    Just x -> do
                        src <- readFile x
                        return $ Right $ snd $ createDatabase Haskell [] src
            Just x -> fmap Right $ loadDatabase x


findFile :: [FilePath] -> IO (Maybe FilePath)
findFile [] = return Nothing
findFile (x:xs) = do
    b <- doesFileExist x
    if b then return $ Just x else findFile xs
