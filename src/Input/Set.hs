{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Set(setStackage, setPlatform, setGHC) where

import Control.Applicative
import Data.List.Extra
import System.IO.Extra
import qualified Data.Set as Set
import Prelude


-- | Return information about which items are in a particular set.
setStackage :: IO (Set.Set String)
setStackage = Set.fromList . f . lines <$> readFile' "input/stackage.txt"
    where
        f (x:xs) | Just x <- stripPrefix "constraints:" x =
                    map (fst . word1) $ takeWhile (" " `isPrefixOf`) $ (' ':x) : xs
                 | otherwise = f xs
        f [] = []


setPlatform :: IO (Set.Set String)
setPlatform = setPlatformWith ["incGHCLib","incLib"]

setPlatformWith :: [String] -> IO (Set.Set String)
setPlatformWith names = do
    src <- lines <$> readFile' "input/platform.txt"
    return $ Set.fromList [read lib | ",":name:lib:_ <- map words src, name `elem` names]

setGHC :: IO (Set.Set String)
setGHC = setPlatformWith ["incGHCLib"]
