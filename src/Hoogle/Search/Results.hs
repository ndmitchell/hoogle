
module Hoogle.Search.Results(
    mergeDataBaseResults, mergeQueryResults
    ) where

import Data.Char
import Data.List
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import General.All

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result


type ResultScore = (Result, RScore)

data RScore = RScore [Score] Int String
              deriving (Eq,Ord)



mergeDataBaseResults :: [[ResultScore]] -> [Result]
mergeDataBaseResults = map fst . fold [] mergeSnd


mergeSnd xs [] = xs
mergeSnd [] ys = ys
mergeSnd (x:xs) (y:ys) | snd x <= snd y = x : mergeSnd xs (y:ys)
                       | otherwise = y : mergeSnd (x:xs) ys


fold :: a -> (a -> a -> a) -> [a] -> a
fold x f [] = x
fold x f xs = fold1 f xs


fold1 :: (a -> a -> a) -> [a] -> a
fold1 f [x] = x
fold1 f xs = f (fold1 f a) (fold1 f b)
    where (a,b) = halves xs


halves [] = ([], [])
halves (x:xs) = (x:b,a)
    where (a,b) = halves xs


-- may be duplicates, eliminate them and order them
mergeQueryResults :: Query -> [[Result]] -> [ResultScore]
mergeQueryResults q = filterResults q . joinResults . map (nubResults . orderResults)


-- join the results of multiple searches
joinResults :: [[ResultScore]] -> [ResultScore]
joinResults [] = []
joinResults [x] = x
joinResults xs = orderResults $ f $ map asSet xs
    where
        asSet = IntMap.fromList . map (\(x,_) -> (entryId $ resultEntry x, x))

        f [x] = IntMap.elems x
        f (x:y:zs) = f (IntMap.intersectionWith g x y : zs)
        g r1 r2 = r1{resultScore = sort $ resultScore r1 ++ resultScore r2
                    ,resultView = resultView r1 ++ resultView r2}



-- | Apply the PlusModule, MinusModule and MinusPackage modes
filterResults :: Query -> [ResultScore] -> [ResultScore]
filterResults q = f mods correctModule . f pkgs correctPackage
    where
        f [] act = id
        f xs act = filter (maybe True (act xs) . resultModPkg . fst)

        mods = filter (\x -> isPlusModule x || isMinusModule x) $ scope q
        pkgs = [x | MinusPackage x <- scope q]


-- pkgs is a non-empty list of MinusPackage values
correctPackage :: [String] -> (Module,Package) -> Bool
correctPackage pkgs = (`notElem` pkgs) . packageName . snd


-- mods is a non-empty list of PlusModule/MinusModule
correctModule :: [Scope] -> (Module,Package) -> Bool
correctModule mods = f base mods . moduleName . fst
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


-- | Put the results in the correct order, by score
orderResults :: [Result] -> [ResultScore]
orderResults = sortScores . map (\x -> (x, f x))
    where
        f r = RScore (resultScore r)
                     (maybe 0 (length . moduleName . fst) $ resultModPkg r)
                     (map toLower $ entryName $ resultEntry r)

sortScores x = sortBy (compare `on` snd) x


-- nub everything having the same entryId
nubResults :: [ResultScore] -> [ResultScore]
nubResults = f IntSet.empty
    where
        f s [] = []
        f s (r:rs) | i `IntSet.member` s = f s rs
                   | otherwise = r : f (IntSet.insert i s) rs
            where i = entryId $ resultEntry $ fst r

