{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Action.Server(spawnMain, test) where

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
import Query hiding (test)
import Input.Type hiding (test)
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
                Just "body" -> OutputString $ if null $ unwords $ grab "hoogle" then welcome else body
        ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
        xs -> return $ OutputFile $ joinPath $ "html" : xs


showResults :: Query -> [ItemEx] -> String
showResults query results = unlines $
    ["<h1>" ++ renderQuery query ++ "</h1>"] ++
    ["<p>No results found</p>" | null results] ++
    ["<div class=result>" ++
     "<div class=ans><a href=\"" ++ itemURL ++ "\">" ++ displayItem query itemItem ++ "</a></div>" ++
     "<div class=from>" ++ unwords ["<a href=\"" ++ b ++ "\">" ++ a ++ "</a>" | (a,b) <- itemParents] ++ "</div>" ++
     "<div class=\"doc newline shut\">" ++ trimStart (replace "<p>" "" $ replace "</p>" "\n" $ unwords $ lines itemDocs) ++ "</div>" ++
     "</div>"
    | ItemEx{..} <- results]


-------------------------------------------------------------
-- DISPLAY AN ITEM (bold keywords etc)

displayItem :: Query -> Item -> String
displayItem Query{..} = keyword . replace "</b><b>" "" . focus
    where
        keyword x | (a,b) <- word1 x, a `elem` kws = "<b>" ++ dropWhile (== '@') a ++ "</b> " ++ b
                  | otherwise = x
            where kws = words "class data type newtype"

        focus (IModule (breakEnd (== '.') -> (pre,post))) =
            "<b>module</b> " ++ escapeHTML pre ++ "<span class=name>" ++ highlight post ++ "</span>"
        focus (IPackage x) = "<b>package</b> <span class=name>" ++ highlight x ++ "</span>"
        focus (IKeyword x) = "<b>keyword</b> <span class=name>" ++ highlight x ++ "</span>"
        focus (IDecl x) | [now] <- declNames x, (pre,stripPrefix now -> Just post) <- breakOn now $ pretty x =
            if "(" `isSuffixOf` pre && ")" `isPrefixOf` post then
                init (escapeHTML pre) ++ "<span class=name>(" ++ highlight now ++ ")</span>" ++ escapeHTML (tail post)
            else
                escapeHTML pre ++ "<span class=name>" ++ highlight now ++ "</span>" ++ escapeHTML post

        highlight :: String -> String
        highlight xs | m > 0, (a,b) <- splitAt m xs = "<b>" ++ escapeHTML a ++ "</b>" ++ highlight b
            where m = maximum $ 0 : [length x | x <- queryName, lower x `isPrefixOf` lower xs]
        highlight (x:xs) = escapeHTML [x] ++ highlight xs
        highlight [] = []


test :: IO ()
test = testing "Action.Server.displayItem" $ do
    let expand = replace "{|" "<span class=name>" . replace "|}" "</span>" . replace "{*" "<b>" . replace "*}" "</b>"
        collapse = replace "{|" "" . replace "|}" "" . replace "{*" "" . replace "*}" ""
    let q === s | Just i <- readItem $ collapse s, displayItem (parseQuery q) i == expand (escapeHTML s) = putChar '.'
                | otherwise = error $ show (q,s,displayItem (parseQuery q) (fromJust $ readItem $ collapse s))
    "test" === "{|my{*Test*}|} :: Int -> test"
    "new west" === "{|{*new*}est_{*new*}|} :: Int" -- FIXME: should ideally highly "est" as well
    "+*" === "{|({*+**}&)|} :: Int"
    "foo" === "{*data*} {|{*Foo*}d|}"
    "foo" === "{*module*} Foo.Bar.{|F{*Foo*}|}"
    "foo" === "{*module*} {|{*Foo*}o|}"
