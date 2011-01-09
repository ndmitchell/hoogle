
module CmdLine.Load(loadQueryDatabases, availableDatabases) where

import Hoogle
import General.Base
import General.Util
import General.System


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
                        src <- readFileUtf8 x
                        return $ Right $ snd $ createDatabase Haskell [] src
            Just x -> fmap Right $ loadDatabase x


findFile :: [FilePath] -> IO (Maybe FilePath)
findFile [] = return Nothing
findFile (x:xs) = do
    b <- doesFileExist x
    if b then return $ Just x else findFile xs


availableDatabases :: [FilePath] -> IO [String]
availableDatabases xs = fmap (sortBy compareString . nub . concat) $ forM xs $ \x -> do
    b <- doesDirectoryExist x
    ys <- if b then getDirectoryContents x else return []
    return [dropExtension y | y <- ys, takeExtension y == ".hoo"] 
