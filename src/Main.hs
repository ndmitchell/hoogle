{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main(main) where

-- grp = 1.28Mb
-- wrd = 10.7Mb

-- [(".grp",1_343_808),(".ids",247_336_269),(".wrd",11_230_952)]
-- [(".grp",1_314_256),(".ids",244_154_208),(".wrd",7_369_220)]


import Language.Haskell.Exts.Annotated
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad.Extra
import System.IO.Unsafe
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Control.Exception.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import Data.Char
import Data.Monoid
import qualified Language.Javascript.JQuery as JQuery

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Input.Cabal
import Input.Hoogle
import Input.Download
import Input.Set
import Query
import Type
import Util
import Web
import System.Mem
import GHC.Stats


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

main :: IO ()
main = do
    args <- getArgs
    let (pkg,rest) = first (map tail) $ span ("@" `isPrefixOf`) args
    if rest == ["-"] then
        spawn $ Database $ "output" </> head (pkg ++ ["all"])
     else if null rest then do
        downloadInputs
        (n,_) <- duration $ generate pkg
        putStrLn $ "Took " ++ showDuration n
     else
        forM_ (if null pkg then ["all"] else pkg) $ \pkg -> do
            res <- search (Database $ "output" </> pkg) $ parseQuery $ unwords rest
            forM_ res $ putStrLn . snd . word1 . head


spawn :: Database -> IO ()
spawn pkg = server 80 $ \Input{..} -> case inputURL of
    [] -> do
        let grab name = [x | (a,x) <- inputArgs, a == name]
        results <- unsafeInterleaveIO $ search pkg $
            parseQuery (unwords $ grab "hoogle") <> Query (map parseRestrict $ grab "restrict") [] Nothing
        let body = show results
        index <- unsafeInterleaveIO $ readFile "html/index.html"
        welcome <- unsafeInterleaveIO $ readFile "html/welcome.html"
        tags <- unsafeInterleaveIO $ concatMap (\x -> "<option" ++ (if x `elem` grab "restrict" then " selected=selected" else "") ++ ">" ++ x ++ "</option>") . listTags <$> readTags pkg
        return $ case lookup "mode" $ reverse inputArgs of
            Nothing | xs@(_:_) <- unwords $ grab "hoogle" -> OutputString $ template [("body",body),("title",xs ++ " - Hoogle"),("search",xs),("tags",tags)] index
                    | otherwise -> OutputString $ template [("body",welcome),("title","Hoogle"),("search",""),("tags",tags)] index
            Just "body" -> OutputString body
    ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
    xs -> return $ OutputFile $ joinPath $ "html" : xs


search :: Database -> Query -> IO [[String]]
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
    mapM (lookupItem pkg) $ take 25 $ filter (filterTags tags qtags) is


generate :: [String] -> IO ()
generate xs = do
    setStackage <- setStackage
    setPlatform <- setPlatform
    setGHC <- setGHC
    createDirectoryIfMissing True "output"
    let want = Set.fromList $ if null xs then setStackage else xs

    cbl <- parseCabal (`Set.member` want)
    files <- if xs /= [] then return ["input/hoogle" </> x <.> "txt" | x <- xs] else
        filterM doesFileExist ["input/hoogle" </> x <.> "txt" | x <- setStackage]
    let n = length files
    inp <- forM (zip [1..] files) $ \(i,file) -> unsafeInterleaveIO $ do
        let pkg = takeBaseName file
        putStrLn $ "[" ++ show i ++ "/" ++ show n ++ "] " ++ pkg
        src <- readFile' file
        return $ ("@set " ++ intercalate ", " (["ghc" | pkg `elem` setGHC] ++ ["platform" | pkg `elem` setPlatform] ++ ["stackage"])) ++ "\n" ++
                 unlines (Map.findWithDefault [] pkg cbl) ++ src
    xs <- return $ parseHoogle $ unlines inp
    let out = "output" </> (if length files == 1 then takeBaseName $ head files else "all")
    xs <- writeFileLefts (out <.> "warn") xs
    xs <- writeItems out xs
    writeTags (Database out) xs
    writeNames (Database out) xs
    writeTypes (Database out) xs

    performGC
    print =<< getGCStats
    evaluate xs

{-
    files <- listFiles "output"
    files <- forM files $ \file -> (takeExtension file,) <$> fileSize file
    let f (name, tot) = name ++ " " ++ reverse (intercalate "," $ chunksOf 3 $ reverse $ show tot)
    putStr $ unlines $ map f $ reverse $ sortOn snd $ map (second sum) $ groupSort files
-}
    print "done"


writeFileLefts :: FilePath -> [Either String a] -> IO [a]
writeFileLefts file xs = do
    ignore $ removeFile file
    f Nothing xs
    where
        f Nothing xs@(Left _:_) = do h <- openFile file WriteMode; f (Just h) xs
        f (Just h) (Left x:xs) = do res <- unsafeInterleaveIO $ hPutStrLn h x; res `seq` f (Just h) xs
        f h (Right x:xs) = fmap (x:) $ f h xs
        f h [] = do whenJust h hClose; return []


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
