{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Action.Server(spawnMain) where

import Control.Applicative
import Data.List.Extra
import System.FilePath
import System.IO.Unsafe
import Data.Monoid
import qualified Language.Javascript.JQuery as JQuery
import Data.Version
import Paths_hogle
import Data.Maybe

import Output.Tags
import Query
import Input.Type
import General.Util
import General.Web
import Action.Search
import Action.CmdLine


spawnMain :: CmdLine -> IO ()
spawnMain Server{..} = do
    let pkg = Database $ "output" </> head ([database | database /= ""] ++ ["all"])
    putStrLn $ "Server started on port " ++ show port
    server port $ \Input{..} -> case inputURL of
        [] -> do
            let grab name = [x | (a,x) <- inputArgs, a == name, x /= ""]
            let q = parseQuery (unwords $ grab "hoogle") <> Query [] Nothing (map parseScope $ grab "scope")
            results <- unsafeInterleaveIO $ search pkg q
            let body = showResults q results
            index <- unsafeInterleaveIO $ readFile "html/index.html"
            welcome <- unsafeInterleaveIO $ readFile "html/welcome.html"
            tags <- unsafeInterleaveIO $ concatMap (\x -> "<option" ++ (if x `elem` grab "scope" then " selected=selected" else "") ++ ">" ++ x ++ "</option>") . listTags <$> readTags pkg
            return $ case lookup "mode" $ reverse inputArgs of
                Nothing | xs@(_:_) <- escapeHTML $ unwords $ grab "hoogle" -> OutputString $ template [("body",body),("title",xs ++ " - Hoogle"),("search",xs),("tags",tags),("version",showVersion version)] index
                        | otherwise -> OutputString $ template [("body",welcome),("title","Hoogle"),("search",""),("tags",tags),("version",showVersion version)] index
                Just "body" -> OutputString body
        ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
        xs -> return $ OutputFile $ joinPath $ "html" : xs


showResults :: Query -> [[String]] -> String
showResults query results = unlines $
    ["<h1>" ++ renderQuery query ++ "</h1>"] ++
    ["<p>No results found</p>" | null results] ++
    ["<div class=result>" ++
     "<div class=ans><a href=\"" ++ b ++ "\">" ++ display (queryName query) (isJust $ queryType query) (snd $ word1 a) ++ "</a></div>" ++
     "<div class=from>" ++ c ++ "</div>" ++
     "<div class=\"doc newline shut\">" ++ trimStart (replace "<p>" "" $ replace "</p>" "\n" $ unwords ds) ++ "</div>" ++
     "</div>"
    | a:b:c:ds <- results]

display :: [String] -> Bool -> String -> String
display names typ item = concatMap f $ groupOn fst $ highlight names $ maybe item prettyItem (readItem item)
    where f xs@((True,_):_) = "<b>" ++ escapeHTML (map snd xs) ++ "</b>"
          f xs = escapeHTML (map snd xs)


highlight :: [String] -> String -> [(Bool, Char)]
highlight names [] = []
highlight names o@(x:xs) | ms == 0 = (False,x) : highlight names xs
                         | (a,b) <- splitAt ms o = map (True,) a ++ highlight names b
    where ms = maximum $ 0 : map length (filter (`isPrefixOf` lower o) $ map lower names)
