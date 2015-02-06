{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Server(actionServer, actionReplay, action_server_test) where

import Prelude(); import General.Prelude
import Data.List.Extra
import System.FilePath
import Control.Exception
import Control.DeepSeq
import Data.Tuple.Extra
import qualified Language.Javascript.JQuery as JQuery
import qualified Language.Javascript.Flot as Flot
import Data.Version
import Paths_hoogle
import Data.Maybe
import Control.Monad
import System.IO.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Time.Extra
import System.Process.Extra
import System.Info.Extra
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid
import Numeric.Extra
import Data.Time.Clock
import System.IO.Unsafe

import Output.Tags
import Query hiding (test)
import Input.Type hiding (test)
import General.Util
import General.Web
import General.Store
import General.Template
import Action.Search
import Action.CmdLine


actionServer :: CmdLine -> IO ()
actionServer Server{..} = do
    let pkg = "output" </> head ([database | database /= ""] ++ ["all"])
    putStrLn $ "Server started on port " ++ show port
    h <- if logs == "" then return stdout else openFile logs AppendMode
    hSetBuffering h LineBuffering
    evaluate time
    storeReadFile (pkg <.> "hoo") $ \store ->
        server h port $ replyServer (Just logs) store cdn

actionReplay :: CmdLine -> IO ()
actionReplay Replay{..} = withBuffering stdout NoBuffering $ do
    src <- readFile logs
    let qs = [readInput url | _:ip:_:url:_ <- map words $ lines src, ip /= "-"]
    (t,_) <- duration $ storeReadFile "output/all.hoo" $ \store -> do
        let op = replyServer Nothing store ""
        forM_ qs $ \x -> do
            res <- op x
            evaluate $ rnf res
            putChar '.'
    putStrLn $ "\nTook " ++ showDuration t ++ " (" ++ showDuration (t / genericLength qs) ++ ")"

{-# NOINLINE time #-}
time :: String
time = unsafePerformIO $ showUTCTime "%Y-%m-%d %H:%M" <$> getCurrentTime

replyServer :: Maybe FilePath -> StoreRead -> String -> Input -> IO Output
replyServer logs store cdn = \Input{..} -> case inputURL of
    -- without -fno-state-hack things can get folded under this lambda
    [] -> do
        let grab name = [x | (a,x) <- inputArgs, a == name, x /= ""]
        let qSource = grab "hoogle" ++ filter (/= "set:stackage") (grab "scope")
        let q = concatMap parseQuery qSource
        let results = search store q
        let body = showResults q $ dedupeTake 25 (\i -> i{itemURL="",itemPackage=Nothing, itemModule=Nothing}) results
        case lookup "mode" $ reverse inputArgs of
            Nothing | qSource /= [] -> fmap OutputString $ templateRender templateIndex $ map (second str)
                        [("tags",tagOptions $ grab "scope"),("body",body),("title",unwords qSource ++ " - Hoogle"),("search",unwords $ grab "hoogle")]
                    | otherwise -> fmap OutputString $ templateRender templateHome []
            Just "body" -> OutputString <$> if null qSource then templateRender templateEmpty [] else return $ LBS.pack body
    ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
    ["plugin","jquery.flot.js"] -> OutputFile <$> Flot.file Flot.Flot
    ["log.txt"] | Just s <- logs -> OutputString . LBS.fromChunks . return <$> readLog s
    ["log.html"] | Just s <- logs -> do
        log <- readLog s
        OutputHTML <$> templateRender templateLog [("data",str $ analyseLog log)]
    xs -> return $ OutputFile $ joinPath $ "html" : xs
    where
        str = templateStr . LBS.pack
        tagOptions sel = concat [tag "option" ["selected=selected" | x `elem` sel] x | x <- listTags $ readTags store]
        params = map (second str)
            [("cdn",cdn),("jquery",if null cdn then "plugin/jquery.js" else JQuery.url)
            ,("version",showVersion version ++ " " ++ time)]
        templateIndex = templateFile "html/index.html" `templateApply` params
        templateEmpty = templateFile "html/welcome.html"
        templateHome = templateIndex `templateApply` [("tags",str $ tagOptions []),("body",templateEmpty),("title",str "Hoogle"),("search",str "")]
        templateLog = templateFile "html/log.html"


readLog :: FilePath -> IO BS.ByteString
readLog file = withTempFile $ \tmp -> do
    systemOutput_ $ (if isWindows then "copy" else "cp") ++ " \"" ++ file ++ "\" \"" ++ tmp ++ "\""
    BS.readFile tmp


dedupeTake :: Ord k => Int -> (v -> k) -> [v] -> [[v]]
dedupeTake n key = f [] Map.empty
    where
        -- map is Map k [v]
        f res mp xs | Map.size mp >= n || null xs = map (reverse . (Map.!) mp) $ reverse res
        f res mp (x:xs) | Just vs <- Map.lookup k mp = f res (Map.insert k (x:vs) mp) xs
                        | otherwise = f (k:res) (Map.insert k [x] mp) xs
            where k = key x 


showResults :: [Query] -> [[ItemEx]] -> String
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

displayItem :: [Query] -> Item -> String
displayItem qs = keyword . focus
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
                  where m = maximum $ 0 : [length y | QueryName y <- qs, lower y `isPrefixOf` lower (x:xs)]
              f (x:xs) = False : f xs
              f [] = []


action_server_test :: IO ()
action_server_test = testing "Action.Server.displayItem" $ do
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


-------------------------------------------------------------
-- ANALYSE THE LOG

data Average = Average !Int !Int deriving Show -- a / b

average (Average a b) = intToDouble a / intToDouble b

instance Monoid Average where
    mempty = Average 0 0
    mappend (Average x1 x2) (Average y1 y2) = Average (x1+y1) (x2+y2)

data Info = Info
    {searchIPs :: !(Set.Set BS.ByteString) -- number of IP addresses doing a hoogle= search
    ,searchCount :: !Int -- number of searches in total
    ,slowestPage :: !Int -- slowest page load time (/ or hoogle=)
    ,averagePage :: !Average -- time taken to display a page on average
    ,errorCount :: !Int -- number of instances of error
    } deriving Show

instance Monoid Info where
    mempty = Info Set.empty 0 0 mempty 0
    mappend (Info x1 x2 x3 x4 x5) (Info y1 y2 y3 y4 y5) =
        Info (x1 <> y1) (x2 + y2) (x3 + y3) (x4 <> y4) (x5 + y5)

-- | Per day
-- number of IP addresses containing hoogle=
-- number of searches containing hoogle=
-- longest page, average search or home page
-- any instances of ERROR
analyseLog :: BS.ByteString -> String
analyseLog = view . foldl' add Map.empty . BS.lines
    where
        view mp = "[" ++ intercalate "," (map f $ Map.toAscList mp) ++ "]"
            where
                f (t, Info{..}) = "{date:" ++ show t ++
                                  ",searchers:" ++ show (Set.size searchIPs) ++ ",searches:" ++ show searchCount ++
                                  ",slowest:" ++ show slowestPage ++ ",average:" ++ show (average averagePage) ++
                                  ",errors:" ++ show errorCount ++ "}"

        add mp (BS.words -> date:ip:(BS.readInt -> Just (time,_)):url)
            | ip /= BS.pack "-" = Map.alter (Just . add2 . fromMaybe mempty) (BS.takeWhile (/= 'T') date) mp
            where
                isPage = isSearch || url == [BS.pack "/"]
                isSearch = any (BS.pack "hoogle=" `BS.isInfixOf`) url
                isError = any (BS.pack "ERROR:" ==) url
                add2 Info{..} = Info
                    (if isSearch then Set.insert ip searchIPs else searchIPs)
                    (searchCount + if isSearch then 1 else 0)
                    (max slowestPage $ if isPage then time else 0)
                    (averagePage <> if isPage then Average time 1 else mempty)
                    (errorCount + if isError then 1 else 0)
        add mp _ = mp
