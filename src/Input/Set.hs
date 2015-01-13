{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Set(setStackage, setPlatform, setGHC) where

import Control.Applicative
import Data.List.Extra


setStackage :: IO [String]
setStackage = f . lines <$> readFile "input/stackage.txt"
    where
        f (x:xs) | Just x <- stripPrefix "constraints:" x =
                    map (fst . word1) $ takeWhile (" " `isPrefixOf`) $ (' ':x) : xs
                 | otherwise = f xs
        f [] = []


setPlatform :: IO [String]
setPlatform = setPlatformWith ["incGHCLib","incLib"]

setPlatformWith :: [String] -> IO [String]
setPlatformWith names = do
    src <- lines <$> readFile "input/platform.txt"
    return [read lib | ",":name:lib:_ <- map words src, name `elem` names]

setGHC :: IO [String]
setGHC = setPlatformWith ["incGHCLib"]
