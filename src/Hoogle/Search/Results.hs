
module Hoogle.Search.Results(
    filterResults, orderResults
    ) where

import Data.Char
import Data.List
import General.All

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result


-- | Apply the PlusModule, MinusModule and MinusPackage modes
filterResults :: Query -> [Result] -> [Result]
filterResults q = f mods correctModule . f pkgs correctPackage
    where
        f [] act = id
        f xs act = filter (maybe True (act xs) . resultModPkg)

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
orderResults :: [Result] -> [Result]
orderResults = map snd . sortBy (compare `on` fst) . map (\x -> (f x, x))
    where
        f r = (resultScore r
              ,maybe 0 (length . moduleName . fst) $ resultModPkg r
              ,map toLower $ entryName $ resultEntry r)

