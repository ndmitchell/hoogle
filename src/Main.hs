{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- grp = 1.28Mb
-- wrd = 10.7Mb

-- [(".grp",1_343_808),(".ids",247_336_269),(".wrd",11_230_952)]
-- [(".grp",1_314_256),(".ids",244_154_208),(".wrd",7_369_220)]


import Language.Haskell.Exts.Annotated
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import Data.Either
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Control.Exception.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import Data.Char

import DataItems
import DataTags
import DataNames
import DataTypes
import ParseCabal
import ParseHoogle
import ParseQuery
import Type
import Util


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

main :: IO ()
main = do
    args <- getArgs
    let (pkg,rest) = first (map tail) $ span ("@" `isPrefixOf`) args
    if null rest then
        generate pkg
     else
        forM_ (if null pkg then ["all"] else pkg) $ \pkg ->
            search (Database $ "output" </> pkg) $ parseQuery $ unwords rest


search :: Database -> Query -> IO ()
search pkg (Query qtags strs typ) = do
    is <- case (strs, typ) of
        ([], Nothing) | null qtags -> putStrLn "No search entered, nothing to do" >> return []
                      | otherwise -> searchNames pkg []
        ([], Just t ) -> searchTypes pkg t
        (xs, Nothing) -> searchNames pkg xs
        (xs, Just t ) -> do
            nam <- Set.fromList <$> searchNames pkg xs
            filter (`Set.member` nam) <$> searchTypes pkg t
    tags <- readTags pkg
    let res = take 25 $ pruneTags tags $ filter (filterTags tags qtags) is
    forM_ res $ \x -> case x of
        Left (a,b) -> putStrLn $ "... plus more things from " ++ a ++ " " ++ b ++ "..."
        Right i -> putStrLn . snd . word1 . head =<< lookupItem pkg i


generate :: [String] -> IO ()
generate xs = do
    setStackage <- lines <$> readFile' "input/set-stackage.txt"
    setPlatform <- lines <$> readFile' "input/set-platform.txt"
    setGHC <- lines <$> readFile' "input/set-ghc.txt"
    files <- if xs /= [] then return ["input/hoogle" </> x <.> "txt" | x <- xs] else
        filterM doesFileExist ["input/hoogle" </> x <.> "txt" | x <- setStackage]
    let n = length files
    forM_ (zip [1..] files) $ \(i,file) -> do
        let pkg = takeBaseName file
        let out = "output" </> pkg
        putStr $ "[" ++ show i ++ "/" ++ show n ++ "] " ++ pkg
        (t,_) <- duration $ do
            cbl <- readFile' $ "input/cabal" </> pkg <.> "cabal"
            src <- readFile' file
            (warns, xs) <- return $ partitionEithers $ parseHoogle $
                ("@set " ++ intercalate ", " (["ghc" | pkg `elem` setGHC] ++ ["platform" | pkg `elem` setPlatform] ++ ["stackage"])) ++ "\n" ++
                unlines (parseCabal cbl) ++ src
            if null warns then ignore $ removeFile $ out <.> "warn" else writeFile (out <.> "warn") $ unlines warns
            xs <- writeItems out xs
            writeTags (Database out) xs
            writeNames (Database out) xs
            writeTypes (Database out) xs
        putStrLn $ " in " ++ show (round t) ++ "s"
    files <- listFiles "output"
    files <- forM files $ \file -> (takeExtension file,) <$> fileSize file
    let f (name, tot) = name ++ " " ++ reverse (intercalate "," $ chunksOf 3 $ reverse $ show tot)
    putStr $ unlines $ map f $ reverse $ sortOn snd $ map (second sum) $ groupSort files
    print "done"


experiment :: IO ()
experiment = do
    files <- listFiles "output"
    types <- fmap concat $ forM (filter ((==) ".types" . takeExtension) files) $ \file -> do
        xs <- readFile' file
        return [x | ParseOk x <- map (parseType . snd . word1) $ lines xs]
    print ("Count", length types)
    print ("Unique", Set.size $ Set.fromList types)
    writeFileBinary "types.txt" $ unlines $ map pretty $ Set.toList $ Set.fromList types
    writeFileBinary "ctors.txt" $ unlines $ map show $ reverse $ sortOn snd $ Map.toList $ Map.fromListWith (+) $ concat [nub [(x:xs,1) | Ident (_ :: SrcSpanInfo) (x:xs) <- universeBi t, isUpper x] | t <- Set.toList $ Set.fromList types]
    writeFileBinary "contexts.txt" $ unlines [pretty t | t <- Set.toList $ Set.fromList types, any ((>1) . length . snd) $ groupSort [(prettyPrint v,cls) | ClassA (_ :: SrcSpanInfo) cls [v] <- universeBi t]]
    error "done"

    src <- BS.readFile "output/bullet.ids"
    forM_ ["Bullet","Disable","Stmt","???"] $ \s -> do
        (t,_) <- duration $ evaluate $ length $ take 50 $ BS.findSubstrings (BS.pack s) src
        print (s, t)
        (t,_) <- duration $ evaluate $ length $ BS.findSubstrings (BS.pack s) src
        print (s, t)
    error "done"


{-


parse

Package docs ["package:cmdargs","author:Neil Mitchell","license:GPL"] [Module docs "System.Console.CmdArgs" ("docs","cmdargs")]

apply the identifier thing comes along

replace all docs with an identifier, which points at the docs

after the heirarchy comes along lift out the innards

heirarchy :: Package Id -> [Stmt]

then text search is only by statement


-}
