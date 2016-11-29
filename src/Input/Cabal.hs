{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, RecordWildCards, ScopedTypeVariables #-}

-- | Module for reading Cabal files.
module Input.Cabal(
    Package(..),
    parseCabalTarball, readGhcPkg,
    packagePopularity, readCabal
    ) where

import Input.Settings

import Data.List.Extra
import System.FilePath
import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.IO.Extra
import General.Str
import System.Process
import System.Directory
import Data.Char
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import General.Util
import General.Conduit
import Data.Monoid
import Control.Applicative
import Prelude

---------------------------------------------------------------------
-- DATA TYPE

-- | A representation of a Cabal package.
data Package = Package
    {packageTags :: [(T.Text, T.Text)] -- ^ The Tag information, e.g. (category,Development) (author,Neil Mitchell).
    ,packageLibrary :: Bool -- ^ True if the package provides a library (False if it is only an executable with no API)
    ,packageSynopsis :: T.Text -- ^ The synposis, grabbed from the top section.
    ,packageVersion :: T.Text -- ^ The version, grabbed from the top section.
    ,packageDepends :: [T.Text] -- ^ The list of packages that this package directly depends on.
    ,packageDocs :: Maybe FilePath -- ^ Directory where the documentation is located
    } deriving Show

instance Monoid Package where
    mempty = Package [] True T.empty T.empty [] Nothing
    mappend (Package x1 x2 x3 x4 x5 x6) (Package y1 y2 y3 y4 y5 y6) =
        Package (x1++y1) (x2||y2) (one x3 y3) (one x4 y4) (nubOrd $ x5 ++ y5) (x6 `mplus` y6)
        where one a b = if T.null a then b else a

instance NFData Package where
    rnf (Package a b c d e f) = rnf (a,b,c,d,e,f)


---------------------------------------------------------------------
-- POPULARITY

-- | Given a set of packages, return the popularity of each package, along with any warnings
--   about packages imported but not found.
packagePopularity :: Map.Map String Package -> ([String], Map.Map String Int)
packagePopularity cbl = (errs, Map.map length good)
    where
        errs =  [ user ++ ".cabal: Import of non-existant package " ++ name ++
                          (if null rest then "" else ", also imported by " ++ show (length rest) ++ " others")
                | (name, user:rest) <- Map.toList bad]
        (good, bad)  = Map.partitionWithKey (\k _ -> k `Map.member` cbl) $
            Map.fromListWith (++) [(T.unpack b,[a]) | (a,bs) <- Map.toList cbl, b <- packageDepends bs]


---------------------------------------------------------------------
-- READERS

-- | Run 'ghc-pkg' and get a list of packages which are installed.
readGhcPkg :: Settings -> IO (Map.Map String Package)
readGhcPkg settings = do
    topdir <- findExecutable "ghc-pkg"
    stdout <- readProcess "ghc-pkg" ["dump"] ""
    let g (stripPrefix "$topdir" -> Just x) | Just t <- topdir = takeDirectory t ++ x
        g x = x
    let fixer p = p{packageLibrary = True, packageDocs = g <$> packageDocs p}
    let f ((stripPrefix "name: " -> Just x):xs) = Just (x, fixer $ readCabal settings $ unlines xs)
        f xs = Nothing
    return $ Map.fromList $ mapMaybe f $ splitOn ["---"] $ lines stdout


-- | Given a tarball of Cabal files, parse the latest version of each package.
parseCabalTarball :: Settings -> FilePath -> IO (Map.Map String Package)
-- items are stored as:
-- QuickCheck/2.7.5/QuickCheck.cabal
-- QuickCheck/2.7.6/QuickCheck.cabal
-- rely on the fact the highest version is last (using lastValues)
parseCabalTarball settings tarfile = do
    res <- runConduit $
        (sourceList =<< liftIO (tarballReadFiles tarfile)) =$=
        mapC (first takeBaseName) =$= groupOnLastC fst =$= mapMC (\x -> do evaluate $ rnf x; return x) =$=
        pipelineC 10 (mapC (second $ readCabal settings . lstrUnpack) =$= mapMC (\x -> do evaluate $ rnf x; return x) =$= sinkList)
    return $ Map.fromList res


---------------------------------------------------------------------
-- PARSERS

-- | Cabal information, plus who I depend on
readCabal :: Settings -> String -> Package
readCabal Settings{..} src = Package{..}
    where
        mp = Map.fromListWith (++) $ lexCabal src
        ask x = Map.findWithDefault [] x mp

        packageDepends =
            map T.pack $ nubOrd $ filter (/= "") $
            map (intercalate "-" . takeWhile (all isAlpha . take 1) . splitOn "-" . fst . word1) $
            concatMap (split (== ',')) (ask "build-depends") ++ concatMap words (ask "depends")
        packageVersion = T.pack $ head $ dropWhile null (ask "version") ++ ["0.0"]
        packageSynopsis = T.pack $ unwords $ words $ unwords $ ask "synopsis"
        packageLibrary = "library" `elem` map (lower . trim) (lines src)
        packageDocs = listToMaybe $ ask "haddock-html"

        packageTags = map (both T.pack) $ nubOrd $ concat
            [ map (head xs,) $ concatMap cleanup $ concatMap ask xs
            | xs <- [["license"],["category"],["author","maintainer"]]]

        -- split on things like "," "&" "and", then throw away email addresses, replace spaces with "-" and rename
        cleanup =
            filter (/= "") .
            map (renameTag . intercalate "-" . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")) .
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
