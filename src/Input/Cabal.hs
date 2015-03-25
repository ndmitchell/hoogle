{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Cabal(parseCabal) where

import Data.List.Extra
import System.FilePath
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import General.Str
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map as Map
import General.Util
import Prelude


-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
-- rely on the fact the highest version is last (using lastValues)
parseCabal :: (String -> Bool) -> IO (Map.Map String [(String, String)])
parseCabal want = do
    rename <- map (both trim . second (drop 1) . break (== '=')) . lines <$> readFile "misc/tag-rename.txt"
    res <- foldl' (f rename) Map.empty . filter (want . fst) . lastValues . map (first takeBaseName) <$> tarballReadFiles "input/cabal.tar.gz"
    evaluate res
    where
        f rename mp (pkg,body) = rnf pkg `seq` rnf res `seq` Map.insert pkg res mp
            where res = extractCabal rename $ lstrUnpack body


lastValues :: Eq a => [(a,b)] -> [(a,b)]
lastValues ((a1,_):(a2,x):xs) | a1 == a2 = lastValues $ (a2,x):xs
lastValues (x:xs) = x : lastValues xs
lastValues [] = []


extractCabal :: [(String, String)] -> String -> [(String,String)]
extractCabal rename src = f ["license"] ++ f ["category"] ++ f ["author","maintainer"]
    where
        f name = nubOrd [ (head name, fromMaybe x $ lookup x rename)
                        | x <- lines src, let (a,b) = break (== ':') x, lower a `elem` name
                         , x <- filter (/= "") $ map g $ concatMap (splitOn "and") $ split (`elem` ",&") $ drop 1 b]
        g = intercalate "-" . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")
