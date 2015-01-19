{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Search(searchMain, search) where

import Control.Applicative
import Data.List.Extra
import System.FilePath
import Control.Monad.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Query
import Input.Type
import Action.CmdLine
import General.Util


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

searchMain :: CmdLine -> IO ()
searchMain Search{..} = do
    let pkg = [database | database /= ""]
    let rest = query
    forM_ (if null pkg then ["all"] else pkg) $ \pkg -> do
        res <- search (Database $ "output" </> pkg) $ parseQuery $ unwords rest
        forM_ res $ putStrLn . snd . word1 . head

search :: Database -> Query -> IO [[String]]
search pkg (Query strs typ qtags) = do
    tags <- readTags pkg
    is <- case (strs, typ) of
        ([], Nothing) | not $ null qtags, xs@(_:_) <- searchTags tags qtags -> return xs
                      | otherwise -> searchNames pkg []
        ([], Just t ) -> searchTypes pkg t
        (xs, Nothing) -> searchNames pkg xs
        (xs, Just t ) -> do
            nam <- Set.fromList <$> searchNames pkg xs
            filter (`Set.member` nam) <$> searchTypes pkg t
    mapM (lookupItem pkg . snd) $ takeScore 25 $ filter (filterTags tags qtags . snd) is

takeScore :: Int -> [(Score, a)] -> [(Score,a)]
takeScore = f 0 Map.empty
    where
        -- Map is Map Score [a], and nmp is the count of items in all list
        f nmp mp i xs | i <= 0 = []
        f nmp mp i [] = take i [(s,y) | (s,ys) <- Map.toAscList mp, y <- ys]
        f nmp mp i xs | nmp > i, Just ((s,ys),mp) <- Map.maxViewWithKey mp =
            let (die,keep) = splitAt (nmp - i) ys
            in f (nmp-length die) (if null keep then mp else Map.insert s keep mp) i xs
        f nmp mp i ((0,x):xs) = (0,x) : f nmp mp (i-1) xs
        f nmp mp i ((s,x):xs) = f (nmp+1) (Map.insertWith (++) s [x] mp) i xs
