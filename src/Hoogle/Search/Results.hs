
module Hoogle.Search.Results(
    mergeDataBaseResults, mergeQueryResults
    ) where

import General.Base
import General.Util
import qualified Data.Map as Map
import Hoogle.Store.All

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


---------------------------------------------------------------------
-- MERGE DATABASE

mergeDataBaseResults :: [[Result]] -> [Result]
mergeDataBaseResults = map fromKey . fold [] merge . map (map $ toKey f)
    where f r = (resultScore r, entryScore $ resultEntry r)


---------------------------------------------------------------------
-- MERGE QUERY

-- each query is correct, elements can be ordered by entry Id
mergeQueryResults :: Query -> [[Result]] -> [Result]
mergeQueryResults q = filterResults q . joinResults


-- join the results of multiple searches
-- FIXME: this looks like a disaster - fully strict
joinResults :: [[Result]] -> [Result]
joinResults [] = []
joinResults [x] = x
joinResults xs = Map.elems $ fold1 (Map.intersectionWith join) $
                 map asSet xs
    where
        asSet = Map.fromList . map (entryUnique . resultEntry &&& id)

        join r1 r2 = r1{resultScore = resultScore r1 <> resultScore r2
                       ,resultView = resultView r1 ++ resultView r2
                       ,resultEntry = resultEntry r1 `entryJoin` resultEntry r2}


---------------------------------------------------------------------
-- FILTER

-- | Apply the PlusModule, MinusModule and MinusPackage modes
filterResults :: Query -> [Result] -> [Result]
filterResults q = f mods (correctModule (exactSearch q)) . f pkgs correctPackage
    where
        f [] act = id
        f xs act = filter (act xs . resultEntry)

        mods = [x | x@(Scope _ Module _) <- scope q]
        pkgs = [x | Scope False Package x <- scope q]


-- pkgs is a non-empty list of MinusPackage values
correctPackage :: [String] -> Entry -> Bool
correctPackage pkgs x = null myPkgs || any (maybe True (`notElem` map (map toLower) pkgs)) myPkgs
    where myPkgs = map (fmap (map toLower . entryName . fromOnce) . listToMaybe . snd) $ entryLocations x


-- mods is a non-empty list of PlusModule/MinusModule
correctModule :: Maybe ItemKind -> [Scope] -> Entry -> Bool
correctModule kind mods x = null myMods || any (maybe True (f base mods)) myMods
    where
        myMods = map (fmap (map (if isJust kind then id else toLower)
                            . entryName . fromOnce)
                      . listToMaybe . drop 1 . snd) $ entryLocations x
        base = case head mods of Scope False Module _ -> True; _ -> False

        f z [] y = z
        f z (Scope b Module x:xs) y
            | doesMatch (map (if isJust kind then id else toLower) x) y = f b xs y
        f z (x:xs) y = f z xs y

        -- match if x is a module starting substring of y
        doesMatch x y = if isJust kind
                        then x == y
                        else x `isPrefixOf` y || ('.':x) `isInfixOf` y
