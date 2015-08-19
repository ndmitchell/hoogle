{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, RecordWildCards #-}

module Input.Cabal(Cabal(..), parseCabalTarball) where

import Data.List.Extra
import System.FilePath
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import System.IO.Extra
import General.Str
import Data.Either.Extra
import Data.Char
import Data.Tuple.Extra
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import General.Util
import Paths_hoogle
import Prelude


data Cabal = Cabal
    {cabalTags :: [(T.Text, T.Text)] -- The Tag information, e.g. (category,Development) (author,Neil Mitchell).
    ,cabalLibrary :: Bool -- True if the package provides a library (False if it is only an executable with no API)
    ,cabalSynopsis :: T.Text -- The synposis, grabbed from the top section.
    ,cabalVersion :: T.Text -- The version, grabbed from the top section. Never empty (will be 0.0 if not found).
    ,cabalPopularity :: !Int -- The number of packages that directly depend on this package.
    } deriving Show

instance NFData Cabal where
    rnf (Cabal a b c d e) = rnf (a,b,c,d,e)


-- | Given the Cabal files we care about, pull out the fields you care about
parseCabalTarball :: FilePath -> IO ([String], Map.Map String Cabal)
-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
-- rely on the fact the highest version is last (using lastValues)
parseCabalTarball tarfile = do
    rename <- loadRename
    -- Left (n, s) -- there are n people importing this package, the first of which was s
    -- Right c -- information about a Cabal package
    let zero = Map.empty :: Map.Map String (Either (Int, String) Cabal)
    res <- foldl' (f rename) zero . lastValues . map (first takeBaseName) <$> tarballReadFiles tarfile
    let (bad, good) = mapPartitionEither res
    let err = [ b ++ ".cabal: Import of non-existant package " ++ a ++ (if i <= 1 then "" else ", also imported by " ++ show (i-1) ++ " others")
              | (a,(i,b)) <- Map.toList bad]
    evaluate good
    evaluate err
    return (err, good)
    where
        setFields new Nothing = Right new
        setFields new (Just (Left (i, _))) = Right new{cabalPopularity = i}

        incPopularity pkg Nothing = Left (1, pkg)
        incPopularity _ (Just (Left (i, pkg))) = i `seq` Left (i+1, pkg)
        incPopularity _ (Just (Right c)) = Right c{cabalPopularity = cabalPopularity c + 1}

        f rename mp (pkg,body) = rnf pkg `seq` rnf res `seq` foldl' (g pkg) (Map.alter (Just . setFields res) pkg mp) deps
            where (res, deps) = readCabal rename $ lstrUnpack body

        g pkg mp dep = Map.alter (Just . incPopularity pkg) dep mp


mapPartitionEither :: Map.Map a (Either b c) -> (Map.Map a b, Map.Map a c)
mapPartitionEither = (Map.map fromLeft *** Map.map fromRight) . Map.partition isLeft


lastValues :: Eq a => [(a,b)] -> [(a,b)]
lastValues ((a1,_):(a2,x):xs) | a1 == a2 = lastValues $ (a2,x):xs
lastValues (x:xs) = x : lastValues xs
lastValues [] = []


loadRename :: IO (String -> String)
loadRename = do
    dataDir <- getDataDir
    src <- readFileUTF8 $ dataDir </> "misc/tag-rename.txt"
    let mp = Map.fromList $ map (both trim . splitPair "=") $ lines src
    return $ \x -> Map.findWithDefault x x mp


-- | Cabal information, plus who I depend on
readCabal :: (String -> String) -> String -> (Cabal, [String])
readCabal rename src = (Cabal{..}, nubOrd depends)
    where
        mp = Map.fromListWith (++) $ lexCabal src
        ask x = Map.findWithDefault [] x mp

        depends = nubOrd $ filter (/= "") $ map (takeWhile $ \x -> isAlphaNum x || x `elem` "-") $
                  concatMap (split (== ',')) $ ask "build-depends"
        cabalVersion = T.pack $ head $ dropWhile null (ask "version") ++ ["0.0"]
        cabalSynopsis = T.pack $ unwords $ words $ unwords $ ask "synopsis"
        cabalLibrary = "library" `elem` map (lower . trim) (lines src)
        cabalPopularity = 0

        cabalTags = map (both T.pack) $ nubOrd $ concat
            [ map (head xs,) $ concatMap cleanup $ concatMap ask xs
            | xs <- [["license"],["category"],["author","maintainer"]]]

        -- split on things like "," "&" "and", then throw away email addresses, replace spaces with "-" and rename
        cleanup =
            filter (/= "") .
            map (rename . intercalate "-" . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")) .
            concatMap (map unwords . split (== "and") . words) . split (`elem` ",&")


-- Ignores nesting beacuse it's not interesting for any of the fields I care about
lexCabal :: String -> [(String, [String])]
lexCabal = f . lines
    where
        f (x:xs) | (white,x) <- span isSpace x
                 , (name@(_:_),x) <- span (\c -> isAlpha c || c == '-') x
                 , ':':x <- trim x
                 , (xs1,xs2) <- span (\s -> length (takeWhile isSpace s) > length white) xs
                 = (lower name, trim x : replace ["."] [""] (map (trim . fst . breakOn "--") xs1)) : f xs2
        f (x:xs) = f xs
        f [] = []
