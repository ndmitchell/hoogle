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
     "<div class=from>" ++ unwords ["<a href=\"" ++ b ++ "\">" ++ a ++ "</a>" | (a,b) <- catMaybes [itemPackage, itemModule]] ++ "</div>" ++
     "<div class=\"doc newline shut\">" ++ trimStart (replace "<p>" "" $ replace "</p>" "\n" $ unwords $ lines itemDocs) ++ "</div>" ++
     "</div>"
    | ItemEx{..} <- results]


-------------------------------------------------------------
-- DISPLAY AN ITEM (bold keywords etc)

displayItem :: Query -> Item -> String
displayItem Query{..} = keyword . focus
    where
        keyword x | (a,b) <- word1 x, a `elem` kws = "<b>" ++ dropWhile (== '@') a ++ "</b> " ++ b
                  | otherwise = x
            where kws = words "class data type newtype"

        name x = "<span class=name>" ++ x ++ "</span>"

        focus (IModule (breakEnd (== '.') -> (pre,post))) =
            "<b>module</b> " ++ escapeHTML pre ++ name (highlight post)
        focus (IPackage x) = "<b>package</b> " ++ name (highlight x)
        focus (IKeyword x) = "<b>keyword</b> " ++ name (highlight x)
        focus (IDecl x) | [now] <- declNames x, (pre,stripPrefix now -> Just post) <- breakOn now $ pretty x =
            if "(" `isSuffixOf` pre && ")" `isPrefixOf` post then
                init (escapeHTML pre) ++ name ("(" ++ highlight now ++ ")") ++ escapeHTML (tail post)
            else
                escapeHTML pre ++ name (highlight now) ++ escapeHTML post

        highlight :: String -> String
        highlight = concatMap (\xs@((b,_):_) -> let s = escapeHTML $ map snd xs in if b then "<b>" ++ s ++ "</b>" else s) .
                    groupOn fst . (\x -> zip (f x) x)
            where
              f (x:xs) | m > 0 = replicate m True ++ drop (m - 1) (f xs)
                  where m = maximum $ 0 : [length y | y <- queryName, lower y `isPrefixOf` lower (x:xs)]
              f (x:xs) = False : f xs
              f [] = []


test :: IO ()
test = testing "Action.Server.displayItem" $ do
    let expand = replace "{|" "<span class=name>" . replace "|}" "</span>" . replace "{*" "<b>" . replace "*}" "</b>"
        collapse = replace "{|" "" . replace "|}" "" . replace "{*" "" . replace "*}" ""
    let q === s | Just i <- readItem $ collapse s, displayItem (parseQuery q) i == expand (escapeHTML s) = putChar '.'
                | otherwise = error $ show (q,s,displayItem (parseQuery q) (fromJust $ readItem $ collapse s))
    "test" === "{|my{*Test*}|} :: Int -> test"
    "new west" === "{|{*newest*}_{*new*}|} :: Int"
    "+*" === "{|({*+**}&)|} :: Int"
    "foo" === "{*data*} {|{*Foo*}d|}"
    "foo" === "{*module*} Foo.Bar.{|F{*Foo*}|}"
    "foo" === "{*module*} {|{*Foo*}o|}"
