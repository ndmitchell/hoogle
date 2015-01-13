{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Cabal(parseCabal) where

import Data.List.Extra
import System.FilePath
import Codec.Compression.GZip as GZip
import Codec.Archive.Tar as Tar
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as LBS


-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
parseCabal :: [String] -> IO (Map.Map String [String])
parseCabal want = do
    src <- LBS.readFile "input/cabal.tar.gz"
    return $ f Map.empty $ Tar.read $ GZip.decompress src
    where
        wanted = Set.fromList want

        -- rely on the fact the highest version is last, and lazy evaluation
        -- skips us from actually parsing the previous values
        f mp (Next e rest) | pkg `Set.member` wanted, NormalFile bs _ <- entryContent e
                           = f (Map.insert pkg (extractCabal $ LBS.unpack bs) mp) rest
            where pkg = takeBaseName $ entryPath e
        f mp (Next _ rest) = f mp rest
        f mp _ = mp

extractCabal :: String -> [String]
extractCabal src = f ["license"] ++ f ["category"] ++ f ["author","maintainer"]
    where
        f name = nub [ "@" ++ head name ++ " " ++ intercalate ", " xs
                     | x <- lines src, let (a,b) = break (== ':') x, lower a `elem` name
                     , let xs = filter (/= "") $ map g $ split (`elem` ",&") $ drop 1 b, not $ null xs]
        g = unwords . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")
