{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Reorder(reorderItems) where

import Input.Type
import Data.List.Extra
import Data.Char
import qualified Data.Map as Map


reorderItems :: [(a, Items)] -> IO [(a, Items)]
reorderItems xs = do
    order <- readOrdering
    return $ concatMap snd $ sortOn (maybe 0 negate . flip Map.lookup order . fst) $ splitPackages xs


splitPackages :: [(a, Items)] -> [(String, [(a, Items)])]
splitPackages = repeatedly $ \(x:xs) ->
    let (a,b) = break (isIPackage . snd) xs
    in ((case snd x of IPackage x -> x; _ -> "", x:a), b)


readOrdering :: IO (Map.Map String Int)
readOrdering = do
    src <- readFile "input/reverse.htm"
    return $ Map.fromList $ f $ lines src
    where
        f (x:"</td>":y:xs)
            | Just x <- stripPrefix "<tr><td><a href=\"http://packdeps.haskellers.com/reverse/" x
            , Just y <- stripPrefix "<td>" y
            = (takeWhile (/= '\"') x, read $ takeWhile isDigit y) : f xs
        f (x:xs) = f xs
        f [] = []
