{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Cabal(parseCabal) where

import Data.List.Extra


parseCabal :: String -> [String]
parseCabal src = f ["license"] ++ f ["category"] ++ f ["author","maintainer"]
    where
        f name = nub [ "@" ++ head name ++ " " ++ intercalate ", " xs
                     | x <- lines src, let (a,b) = break (== ':') x, lower a `elem` name
                     , let xs = filter (/= "") $ map g $ split (`elem` ",&") $ drop 1 b, not $ null xs]
        g = unwords . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")
