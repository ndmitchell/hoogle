{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Search(searchMain, search) where

import Control.Applicative
import System.FilePath
import Control.Monad.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Tuple.Extra
import Data.List

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
        forM_ res $ putStrLn . prettyItem . itemItem

search :: Database -> Query -> IO [ItemEx]
search pkg (Query strs typ qtags) = do
    tags <- readTags pkg
    let exact = Scope True "is" "exact" `elem` qtags
    is <- case (strs, typ) of
        ([], Nothing) | not $ null qtags, xs@(_:_) <- searchTags tags qtags -> return xs
                      | otherwise -> searchNames pkg exact []
        ([], Just t ) -> searchTypes pkg t
        (xs, Nothing) -> searchNames pkg exact xs
        (xs, Just t ) -> do
            nam <- Set.fromList <$> searchNames pkg exact xs
            filter (`Set.member` nam) <$> searchTypes pkg t
    look <- lookupItem pkg
    mapM (look . snd) $ takeScore 25 $ filter (filterTags tags qtags . snd) is

takeScore :: Int -> [(Score, a)] -> [(Score,a)]
takeScore = f 0 Map.empty
    where
        -- Map is Map Score [a], and nmp is the count of items in all list
        f nmp mp i xs | i <= 0 = []
        f nmp mp i [] = take i [(s,y) | (s,ys) <- Map.toAscList mp, y <- reverse ys]
        f nmp mp i xs | nmp > i, Just ((s,ys),mp) <- Map.maxViewWithKey mp =
            let (die,keep) = splitAt (nmp - i) ys
            in f (nmp-length die) (if null keep then mp else Map.insert s keep mp) i xs
        f nmp mp i ((0,x):xs) = (0,x) : f nmp mp (i-1) xs
        f nmp mp i ((s,x):xs) = f (nmp+1) (Map.insertWith (++) s [x] mp) i xs



---------------------------------------------------------------------
-- SCORER

_scorer :: (Ord hash, Ord tag) => Int -> Int -> [(Score, value, hash, [tag])] -> ([(Score, [value])], [(tag, Int)])
_scorer limitVals limitTags = (valuesScorer &&& tagsScorer) . foldl' f (newScorer limitVals limitTags)
    where f mp (s,v,h,ts) = delScorer $ addScorer s v h ts mp

data Scorer value hash tag = Scorer
    {limitVals :: Int -- maximum count for the result values
    ,limitTags :: Int -- maximum count for any tag
    ,maxScore :: Maybe Score -- highest score in the map already (cache of maxView)
    ,mpVals :: Map.Map (Score, Int) (hash, [value]) -- the result I'm building up
    ,mpSeen :: Map.Map hash (Score, Int)            -- hash is for values which are already stored and where
    ,mpTags :: Map.Map tag Int                      -- how many times have I seen a tag (may be greater than limitTags)
    }

newScorer :: Int -> Int -> Scorer value hash tag
newScorer limitVals limitTags = Scorer limitVals limitTags Nothing Map.empty Map.empty Map.empty

valuesScorer :: Scorer value hash tag -> [(Score, [value])]
valuesScorer = map (fst *** (reverse . snd)) . Map.toAscList . mpVals

tagsScorer :: Scorer value hash tag -> [(tag, Int)] -- how many time I've seen a tag
tagsScorer = Map.toList . mpTags

addScorer :: (Ord hash, Ord tag) => Score -> value -> hash -> [tag] -> Scorer value hash tag -> Scorer value hash tag
addScorer s v h ts o@Scorer{..}
    | Map.size mpVals >= limitVals, Just s >= maxScore = o -- too high to possibly get in
    | Just si <- Map.lookup h mpSeen = o{mpVals = Map.adjust (second (v:)) si mpVals}


delScorer :: Scorer value hash tag -> Scorer value hash tag
delScorer o = o
