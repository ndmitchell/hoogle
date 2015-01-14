{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Cabal(parseCabal) where

import Data.List.Extra
import System.FilePath
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as LBS
import Util


-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
parseCabal :: [String] -> IO (Map.Map String [String])
parseCabal want = foldl f Map.empty <$> tarballReadFiles "input/cabal.tar.gz"
    where
        wanted = Set.fromList want

        -- rely on the fact the highest version is last, and lazy evaluation
        -- skips us from actually parsing the previous values
        f mp (name, body) | pkg `Set.member` wanted = Map.insert pkg (extractCabal $ LBS.unpack body) mp
                          | otherwise = mp
            where pkg = takeBaseName name

extractCabal :: String -> [String]
extractCabal src = f ["license"] ++ f ["category"] ++ f ["author","maintainer"]
    where
        f name = nub [ "@" ++ head name ++ " " ++ intercalate ", " xs
                     | x <- lines src, let (a,b) = break (== ':') x, lower a `elem` name
                     , let xs = filter (/= "") $ map g $ split (`elem` ",&") $ drop 1 b, not $ null xs]
        g = unwords . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")
