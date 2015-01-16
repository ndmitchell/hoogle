{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main(main) where

import Control.Applicative
import Data.List.Extra
import System.FilePath
import Control.Monad.Extra
import System.IO.Unsafe
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Exception.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid
import qualified Language.Javascript.JQuery as JQuery
import Debug.Trace

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Input.Cabal
import Input.Hoogle
import Input.Download
import Input.Set
import Query
import Input.Type
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
        let grab name = [x | (a,x) <- inputArgs, a == name, x /= ""]
        results <- unsafeInterleaveIO $ search pkg $
            parseQuery (unwords $ grab "hoogle") <> Query (map parseRestrict $ grab "restrict") [] Nothing
        let body = showResults (unwords $ grab "hoogle" ++ grab "restrict") results
        index <- unsafeInterleaveIO $ readFile "html/index.html"
        welcome <- unsafeInterleaveIO $ readFile "html/welcome.html"
        tags <- unsafeInterleaveIO $ concatMap (\x -> "<option" ++ (if x `elem` grab "restrict" then " selected=selected" else "") ++ ">" ++ x ++ "</option>") . listTags <$> readTags pkg
        return $ case lookup "mode" $ reverse inputArgs of
            Nothing | xs@(_:_) <- escapeHTML $ unwords $ grab "hoogle" -> OutputString $ template [("body",body),("title",xs ++ " - Hoogle"),("search",xs),("tags",tags)] index
                    | otherwise -> OutputString $ template [("body",welcome),("title","Hoogle"),("search",""),("tags",tags)] index
            Just "body" -> OutputString body
    ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
    xs -> return $ OutputFile $ joinPath $ "html" : xs


showResults :: String -> [[String]] -> String
showResults query results = unlines $
    ["<h1>" ++ escapeHTML query ++ "</h1>"] ++
    ["<p>No results found</p>" | null results] ++
    ["<div class=ans><a href=\"" ++ b ++ "\">" ++ escapeHTML (snd $ word1 a) ++ "</a></div><div class=from>" ++ c ++ "</div><div class=\"doc newline shut\">" ++ replace "<p>" "" (replace "</p>" "<br/>" $ unlines ds) ++ "</div>" | a:b:c:ds <- results]


search :: Database -> Query -> IO [[String]]
search pkg (Query qtags strs typ) = do
    tags <- readTags pkg
    is <- case (strs, typ) of
        ([], Nothing) | null qtags -> putStrLn "No search entered, nothing to do" >> return []
                      | xs@(_:_) <- searchTags tags qtags -> return xs
                      | otherwise -> searchNames pkg []
        ([], Just t ) -> searchTypes pkg t
        (xs, Nothing) -> searchNames pkg xs
        (xs, Just t ) -> do
            nam <- Set.fromList <$> searchNames pkg xs
            filter (`Set.member` nam) <$> searchTypes pkg t
    mapM (lookupItem pkg) $ take 25 $ filter (filterTags tags qtags) is


generate :: [String] -> IO ()
generate xs = do
    setStackage <- setStackage
    setPlatform <- setPlatform
    setGHC <- setGHC
    createDirectoryIfMissing True "output"
    let want = Set.fromList $ if null xs then setStackage else xs

    cbl <- parseCabal (`Set.member` want)
    let f seen (takeBaseName -> pkg, body)
            | pkg `Set.member` want
            = (Set.insert pkg seen, trace ("[" ++ show (Set.size seen + 1) ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $ unlines $
                    ("@set " ++ intercalate ", " (["included-with-ghc" | pkg `elem` setGHC] ++ ["haskell-platform" | pkg `elem` setPlatform] ++ ["stackage"])) :
                    Map.findWithDefault [] pkg cbl ++ [LBS.unpack body])
        f seen _ = (seen, "")
    (seen, xs) <- second (parseHoogle . unlines) . mapAccumL f Set.empty <$> tarballReadFiles "input/hoogle.tar.gz"
    let out = "output" </> (if Set.size want == 1 then head $ Set.toList want else "all")
--    xs <- writeFileLefts (out <.> "warn") xs
    xs <- writeItems out [x | Right x <- xs]
    putStrLn $ "Packages not found: " ++ unwords (Set.toList $ want `Set.difference` seen)
    writeTags (Database out) xs
    writeNames (Database out) xs
    writeTypes (Database out) xs

    performGC
    print =<< getGCStats
    evaluate xs
    print "done"

{-
writeFileLefts :: FilePath -> [Either String a] -> IO [a]
writeFileLefts file xs = do
    ignore $ removeFile file
    f Nothing xs
    where
        f Nothing xs@(Left _:_) = do h <- openBinaryFile file WriteMode; f (Just h) xs
        f (Just h) (Left x:xs) = do res <- unsafeInterleaveIO $ hPutStrLn h x; res `seq` f (Just h) xs
        f h (Right x:xs) = fmap (x:) $ f h xs
        f h [] = do whenJust h hClose; return []
-}
