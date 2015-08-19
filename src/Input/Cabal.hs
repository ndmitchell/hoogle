{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, RecordWildCards, BangPatterns, ScopedTypeVariables #-}

module Input.Cabal(Cabal(..), parseCabalTarball) where

import Data.List.Extra
import System.FilePath
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import System.IO.Extra
import Control.Monad.Extra
import General.Str
import Data.Char
import Data.Tuple.Extra
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import General.Util
import General.Conduit
import Paths_hoogle
import Prelude


data Cabal = Cabal
    {cabalTags :: [(T.Text, T.Text)] -- The Tag information, e.g. (category,Development) (author,Neil Mitchell).
    ,cabalLibrary :: Bool -- True if the package provides a library (False if it is only an executable with no API)
    ,cabalSynopsis :: T.Text -- The synposis, grabbed from the top section.
    ,cabalVersion :: T.Text -- The version, grabbed from the top section. Never empty (will be 0.0 if not found).
    ,cabalPopularity :: {-# UNPACK #-} !Int -- The number of packages that directly depend on this package.
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

    runConduit $ (sourceList =<< liftIO (tarballReadFiles tarfile)) =$=
                 mapC (first takeBaseName) =$= groupOnLastC fst =$= mapMC (\x -> do evaluate $ rnf x; return x) =$=
                 pipelineC 10 (mapC (second $ readCabal rename . lstrUnpack) =$= mapMC (\x -> do evaluate $ rnf x; return x) =$= mergeCabals)


mergeCabals :: Monad m => Consumer (String, (Cabal, [String])) m ([String], Map.Map String Cabal)
mergeCabals = freeze <$> foldC add (Map.empty, Map.empty)
    where
        -- Takes imports (Map Name (Int,Name)), where a |-> (i,b) means a has i people who depend on it, b is one of them
        -- Takes cabals (Map Name Cabal), just the mapping to return at the end
        add (!imports, !cabals) x@(name,(cabal,depends)) = (imports2, Map.insert name cabal cabals)
            where imports2 = foldl' (\mp i -> Map.insertWith merge i (1::Int,name) mp) imports depends
                  merge (c1,n1) (c2,n2) = let cc = c1+c2 in cc `seq` (cc, n1)

        freeze (imports, cabals) = bad `seq` good `seq` (bad, good)
            where bad = [ user ++ ".cabal: Import of non-existant package " ++ name ++
                          (if i <= 1 then "" else ", also imported by " ++ show (i-1) ++ " others")
                        | (name,(i,user)) <- Map.toList $ Map.difference imports cabals]
                  good = Map.differenceWith (\c (i,_) -> Just c{cabalPopularity=i}) cabals imports


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
