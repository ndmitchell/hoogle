{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Reorder(reorderItems) where

import Input.Type
import Data.List.Extra
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map as Map


reorderItems :: [(a, Item)] -> IO [(a, Item)]
reorderItems xs = do
    packageOrder <- packageOrder
    let rebase (x, xs) | x `elem` ["base","haskell98","haskell2010"]
                       = (x, concatMap snd $ sortOn ((baseModuleOrder &&& id) . fst) $ splitIModule xs)
        rebase (x, xs) = (x, concatMap snd $ sortOn fst $ splitIModule xs)
    return $ concatMap snd $ sortOn ((packageOrder &&& id) . fst) $ map rebase $ splitIPackage xs


baseModuleOrder :: String -> Int
baseModuleOrder x
    | "GHC." `isPrefixOf` x = maxBound
    | otherwise = fromMaybe (maxBound-1) $ elemIndex x
    ["Prelude","Data.List","Data.Maybe","Data.Function","Control.Monad","List","Maybe","Monad"]

packageOrder :: IO (String -> Int)
packageOrder = do
    src <- readFile "input/reverse.htm"
    let mp = Map.fromList $ f $ lines src
    return $ maybe 0 negate . flip Map.lookup mp
    where
        f (x:"</td>":y:xs)
            | Just x <- stripPrefix "<tr><td><a href=\"http://packdeps.haskellers.com/reverse/" x
            , Just y <- stripPrefix "<td>" y
            = (takeWhile (/= '\"') x, read $ takeWhile isDigit y) : f xs
        f (x:xs) = f xs
        f [] = []
