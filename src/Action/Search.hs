{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Search(searchMain, search) where

import Control.Applicative
import Data.List.Extra
import System.FilePath
import Control.Monad.Extra
import qualified Data.Set as Set

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Query
import Input.Type


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

searchMain :: [String] -> [String] -> IO ()
searchMain pkg rest =
    forM_ (if null pkg then ["all"] else pkg) $ \pkg -> do
        res <- search (Database $ "output" </> pkg) $ parseQuery $ unwords rest
        forM_ res $ putStrLn . snd . word1 . head

search :: Database -> Query -> IO [[String]]
search pkg (Query qtags strs typ) = do
    tags <- readTags pkg
    is <- case (strs, typ) of
        ([], Nothing) | null qtags -> putStrLn "No search entered, nothing to do" >> return []
                      | xs@(_:_) <- searchTags tags qtags -> return xs
                      | otherwise -> searchNames pkg []
        ([], Just t ) -> searchTypes pkg t
        (xs, Nothing) -> searchNames pkg xs
        (xs, Just t ) -> do
            nam <- Set.fromList <$> searchNames pkg xs
            filter (`Set.member` nam) <$> searchTypes pkg t
    mapM (lookupItem pkg) $ take 25 $ filter (filterTags tags qtags) is
