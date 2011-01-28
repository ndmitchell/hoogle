
module Hoogle.Search.Results(
    mergeDataBaseResults, mergeQueryResults
    ) where

import General.Base
import General.Util
import qualified Data.IntMap as IntMap
import Data.Binary.Defer.Index

import Hoogle.Type.All
import Hoogle.Query.All


---------------------------------------------------------------------
-- KEYS

data Key k v = Key k v

instance Eq k => Eq (Key k v) where
    Key k1 v1 == Key k2 v2 = k1 == k2

instance Ord k => Ord (Key k v) where
    compare (Key k1 v1) (Key k2 v2) = compare k1 k2

toKey f v = Key (f v) v
fromKey (Key k v) = v
sortWith f = map fromKey . sort . map (toKey f)


---------------------------------------------------------------------
-- MERGE DATABASE

mergeDataBaseResults :: [[Result]] -> [Result]
mergeDataBaseResults = map fromKey . fold [] merge . map (map $ toKey f)
    where f r = (resultScore r, entryScore $ fromLink $ resultEntry r)


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

        join r1 r2 = r1{resultScore = mappend (resultScore r1) (resultScore r2)
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

        mods = filter isMod $ scope q
        pkgs = [x | MinusPackage x <- scope q]

        isMod PlusModule{}  = True
        isMod MinusModule{} = True
        isMod _ = False


-- pkgs is a non-empty list of MinusPackage values
correctPackage :: [String] -> Entry -> Bool
correctPackage pkgs x = null myPkgs || any (maybe True (`notElem` pkgs)) myPkgs
    where myPkgs = map (fmap (entryName . fromLink) . listToMaybe . snd) $ entryLocations x


-- mods is a non-empty list of PlusModule/MinusModule
correctModule :: [Scope] -> Entry -> Bool
correctModule mods x = null myMods || any (maybe True (f base mods)) myMods
    where
        myMods = map (fmap (map toLower . entryName . fromLink) . listToMaybe . drop 1 . snd) $
                 entryLocations x
        base = case head mods of MinusModule{} -> True; _ -> False

        f z [] y = z
        f z (PlusModule  x:xs) y | doesMatch (map toLower x) y = f True  xs y
        f z (MinusModule x:xs) y | doesMatch (map toLower x) y = f False xs y
        f z (x:xs) y = f z xs y

        -- match if x is a module starting substring of y
        doesMatch x y = x `isPrefixOf` y || ('.':x) `isInfixOf` y
