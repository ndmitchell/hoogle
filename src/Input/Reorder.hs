{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Reorder(reorderItems) where

import Input.Type
import Data.List.Extra
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map as Map


reorderItems :: [(a, Items)] -> IO [(a, Items)]
reorderItems xs = do
    packageOrder <- packageOrder
    let rebase ("base",xs) = ("base", concatMap snd $ sortOn ((baseModuleOrder &&& id) . fst) $ splitModules xs)
        rebase (x, xs) = (x, concatMap snd $ sortOn fst $ splitModules xs)
    return $ concatMap snd $ sortOn ((packageOrder &&& id) . fst) $ map rebase $ splitPackages xs


splitPackages = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitModules = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe String) -> [a] -> [(String, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe "" $ f x, x:a), b)


baseModuleOrder :: String -> Int
baseModuleOrder x
    | "GHC." `isPrefixOf` x = maxBound
    | otherwise = fromMaybe (maxBound-1) $ elemIndex x
    ["Prelude","Data.List","Data.Maybe","Data.Function","Control.Monad"]

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
