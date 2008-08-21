
module Hoogle.Search.Results(
    mergeDataBaseResults, mergeQueryResults
    ) where

import General.Code
import Data.Key
import qualified Data.IntMap as IntMap
import Data.Binary.Defer.Index

import Hoogle.Item.All
import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result


---------------------------------------------------------------------
-- MERGE DATABASE

mergeDataBaseResults :: [[Result]] -> [Result]
mergeDataBaseResults = map fromKey . fold [] merge . map (map $ toKey f)
    where f r = (resultScore r, entryScore $ fromLink $ resultEntry r)


entryScoreModule :: Maybe Module -> Entry -> EntryScore
entryScoreModule mod e = EntryScore
    (length m) (map toLower $ entryName e) (entryName e) m
    where m = maybe [] moduleName mod


---------------------------------------------------------------------
-- MERGE QUERY

-- each query is correct, elements can be ordered by entry Id
mergeQueryResults :: Query -> [[Result]] -> [Result]
mergeQueryResults q = filterResults q . joinResults


-- join the results of multiple searches
joinResults :: [[Result]] -> [Result]
joinResults [] = []
joinResults [x] = x
joinResults xs = sortWith scr $ IntMap.elems $
                 fold1 (IntMap.intersectionWith join) $
                 map asSet xs
    where
        asSet = IntMap.fromList . map (linkKey . resultEntry &&& id)

        join r1 r2 = r1{resultScore = sort $ resultScore r1 ++ resultScore r2
                       ,resultView = resultView r1 ++ resultView r2}

        scr = resultScore &&& linkKey . resultEntry


---------------------------------------------------------------------
-- FILTER

-- | Apply the PlusModule, MinusModule and MinusPackage modes
filterResults :: Query -> [Result] -> [Result]
filterResults q = f mods correctModule . f pkgs correctPackage
    where
        f [] act = id
        f xs act = filter (act xs . fromLink . resultEntry)

        mods = filter (\x -> isPlusModule x || isMinusModule x) $ scope q
        pkgs = [x | MinusPackage x <- scope q]


-- pkgs is a non-empty list of MinusPackage values
correctPackage :: [String] -> Entry -> Bool
correctPackage pkgs = (`notElem` pkgs) . packageName . fromLink . entryPackage


-- mods is a non-empty list of PlusModule/MinusModule
correctModule :: [Scope] -> Entry -> Bool
correctModule mods = maybe True (f base mods . moduleName . fromLink) . entryModule
    where
        base = isMinusModule $ head mods

        f z [] y = z
        f z (PlusModule  x:xs) y | doesMatch x y = f True  xs y
        f z (MinusModule x:xs) y | doesMatch x y = f False xs y
        f z (x:xs) y = f z xs y

        -- match if x is further up the tree than y
        doesMatch [] y = True
        doesMatch (x:xs) (y:ys) = x == y && doesMatch xs ys
        doesMatch _ _ = False
