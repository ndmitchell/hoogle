{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Action.Server(actionServer, actionReplay, test) where

import Prelude(); import General.Prelude
import Data.List.Extra
import System.FilePath
import System.IO.Unsafe
import Control.Exception
import Control.DeepSeq
import qualified Language.Javascript.JQuery as JQuery
import Data.Version
import Paths_hogle
import Data.Maybe
import Control.Monad
import System.IO.Extra
import qualified Data.Map as Map

import Output.Tags
import Query hiding (test)
import Input.Type hiding (test)
import General.Util
import General.Web
import Action.Search
import Action.CmdLine


actionServer :: CmdLine -> IO ()
actionServer Server{..} = do
    let pkg = Database $ "output" </> head ([database | database /= ""] ++ ["all"])
    putStrLn $ "Server started on port " ++ show port
    h <- if logs == "" then return stdout else openFile logs AppendMode
    hSetBuffering h LineBuffering
    server h port $ replyServer pkg

actionReplay :: CmdLine -> IO ()
actionReplay Replay{..} = withBuffering stdout NoBuffering $ do
    src <- readFile logs
    forM_ [readInput url | _:ip:_:url:_ <- map words $ lines src, ip /= "-"] $ \x -> do
        res <- replyServer (Database "output/all") x
        evaluate $ rnf res
        putChar '.'
    putStrLn ""

replyServer :: Database -> Input -> IO Output
replyServer pkg Input{..} = case inputURL of
    [] -> do
        let grab name = [x | (a,x) <- inputArgs, a == name, x /= ""]
        let qSource = grab "hoogle" ++ filter (/= "set:stackage") (grab "scope")
        let q = mconcat $ map parseQuery qSource
        results <- unsafeInterleaveIO $ search pkg q
        let body = showResults q $ dedupeTake 25 (\i -> i{itemURL="",itemPackage=Nothing, itemModule=Nothing}) results
        index <- unsafeInterleaveIO $ readFile "html/index.html"
        welcome <- unsafeInterleaveIO $ readFile "html/welcome.html"
        tags <- unsafeInterleaveIO $ concatMap (\x -> "<option" ++ (if x `elem` grab "scope" then " selected=selected" else "") ++ ">" ++ x ++ "</option>") . listTags <$> readTags pkg
        return $ case lookup "mode" $ reverse inputArgs of
            Nothing | qSource /= [] -> OutputString $ template [("body",body),("title",unwords qSource ++ " - Hoogle"),("search",unwords $ grab "hoogle"),("tags",tags),("version",showVersion version)] index
                    | otherwise -> OutputString $ template [("body",welcome),("title","Hoogle"),("search",""),("tags",tags),("version",showVersion version)] index
            Just "body" -> OutputString $ if null qSource then welcome else body
    ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
    xs -> return $ OutputFile $ joinPath $ "html" : xs


dedupeTake :: Ord k => Int -> (v -> k) -> [v] -> [[v]]
dedupeTake n key = f [] Map.empty
    where
        -- map is Map k [v]
        f res mp xs | Map.size mp >= n || null xs = map (reverse . (Map.!) mp) $ reverse res
        f res mp (x:xs) | Just vs <- Map.lookup k mp = f res (Map.insert k (x:vs) mp) xs
                        | otherwise = f (k:res) (Map.insert k [x] mp) xs
            where k = key x 



showResults :: Query -> [[ItemEx]] -> String
showResults query results = unlines $
    ["<h1>" ++ renderQuery query ++ "</h1>"] ++
    ["<p>No results found</p>" | null results] ++
    ["<div class=result>" ++
     "<div class=ans><a href=\"" ++ itemURL ++ "\">" ++ displayItem query itemItem ++ "</a></div>" ++
     "<div class=from>" ++ showFroms is  ++ "</div>" ++
     "<div class=\"doc newline shut\">" ++ trimStart (replace "<p>" "" $ replace "</p>" "\n" $ unwords $ lines itemDocs) ++ "</div>" ++
     "</div>"
    | is@(ItemEx{..}:_) <- results]

showFroms :: [ItemEx] -> String
showFroms xs = intercalate ", " $ for pkgs $ \p ->
    let ms = filter ((==) p . itemPackage) xs
    in unwords ["<a href=\"" ++ b ++ "\">" ++ a ++ "</a>" | (a,b) <- catMaybes $ p : map remod ms]
    where
        remod ItemEx{..} = do (a,_) <- itemModule; return (a,itemURL)
        pkgs = nub $ map itemPackage xs

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
