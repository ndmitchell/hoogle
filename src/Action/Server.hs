{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Server(actionServer, actionReplay, action_server_test_, action_server_test) where

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
import General.Str
import qualified Data.Map as Map
import System.Time.Extra
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Unsafe
import Numeric.Extra
import GHC.Stats
import System.Info.Extra

import Output.Tags
import Query
import Input.Item
import General.Util
import General.Web
import General.Store
import General.Template
import General.Log
import Action.Search
import Action.CmdLine
import Control.Applicative
import Prelude

import qualified Data.Aeson as JSON


actionServer :: CmdLine -> IO ()
actionServer cmd@Server{..} = do
    -- so I can get good error messages
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    putStrLn $ "Server started on port " ++ show port
    putStr "Reading log..." >> hFlush stdout
    time <- offsetTime
    log <- logCreate (if logs == "" then Left stdout else Right logs) $
        \x -> "hoogle=" `isInfixOf` x && not ("is:ping" `isInfixOf` x)
    putStrLn . showDuration =<< time
    evaluate spawned
    dataDir <- getDataDir
    withSearch database $ \store ->
        server log cmd $ replyServer log local store cdn home (dataDir </> "html") scope

actionReplay :: CmdLine -> IO ()
actionReplay Replay{..} = withBuffering stdout NoBuffering $ do
    src <- readFile logs
    let qs = [readInput url | _:ip:_:url:_ <- map words $ lines src, ip /= "-"]
    (t,_) <- duration $ withSearch database $ \store -> do
        log <- logNone
        dataDir <- getDataDir
        let op = replyServer log False store "" "" (dataDir </> "html") scope
        replicateM_ repeat_ $ forM_ qs $ \x -> do
            res <- op x
            evaluate $ rnf res
            putChar '.'
    putStrLn $ "\nTook " ++ showDuration t ++ " (" ++ showDuration (t / intToDouble (repeat_ * length qs)) ++ ")"

{-# NOINLINE spawned #-}
spawned :: UTCTime
spawned = unsafePerformIO getCurrentTime

replyServer :: Log -> Bool -> StoreRead -> String -> String -> FilePath -> String -> Input -> IO Output
replyServer log local store cdn home htmlDir scope = \Input{..} -> case inputURL of
    -- without -fno-state-hack things can get folded under this lambda
    [] -> do
        let grab name = [x | (a,x) <- inputArgs, a == name, x /= ""]
        let qScope = let xs = grab "scope" in [scope | null xs && scope /= ""] ++ xs
        let qSource = grab "hoogle" ++ filter (/= "set:stackage") qScope
        let q = concatMap parseQuery qSource
        let (q2, results) = search store q
        let body = showResults local inputArgs q2 $ dedupeTake 25 (\t -> t{targetURL="",targetPackage=Nothing, targetModule=Nothing}) results
        case lookup "mode" $ reverse inputArgs of
            Nothing | qSource /= [] -> fmap OutputString $ templateRender templateIndex $ map (second str)
                        [("tags",tagOptions qScope)
                        ,("body",body)
                        ,("title",unwords qSource ++ " - Hoogle")
                        ,("search",unwords $ grab "hoogle")
                        ,("robots",if any isQueryScope q then "none" else "index")]
                    | otherwise -> OutputString <$> templateRender templateHome []
            Just "body" -> OutputString <$> if null qSource then templateRender templateEmpty [] else return $ lstrPack body
            Just "json" -> return $ OutputJSON $ JSON.encode $ take 100 results
            Just m -> return $ OutputFail $ lstrPack $ "Mode " ++ m ++ " not (currently) supported"
    ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
    ["plugin","jquery.flot.js"] -> OutputFile <$> Flot.file Flot.Flot
    ["plugin","jquery.flot.time.js"] -> OutputFile <$> Flot.file Flot.FlotTime
    ["canary"] -> do
        now <- getCurrentTime
        summ <- logSummary log
        let errs = sum [summaryErrors | Summary{..} <- summ, summaryDate >= pred (utctDay now)]
        let alive = fromRational $ toRational $ (now `diffUTCTime` spawned) / (24 * 60 * 60)
        let s = show errs ++ " errors since yesterday, running for " ++ showDP 2 alive ++ " days."
        return $ if errs == 0 && alive < 1.5 then OutputString $ lstrPack $ "Happy. " ++ s else OutputFail $ lstrPack $ "Sad. " ++ s
    ["log"] -> do
        log <- displayLog <$> logSummary log
        OutputHTML <$> templateRender templateLog [("data",str log)]
    ["stats"] -> do
        stats <- getGCStatsEnabled
        if stats then do
            x <- getGCStats
            return $ OutputString $ lstrPack $ replace ", " "\n" $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') $ show x
         else
            return $ OutputFail $ lstrPack "GHC Statistics is not enabled, restart with +RTS -T"
    "file":xs | local -> do
        let x = ['/' | not isWindows] ++ intercalate "/" xs
        return $ OutputFile $ x ++ (if hasTrailingPathSeparator x then "index.html" else "")
    xs ->
        -- avoid "" and ".." in the URLs, since they could be trying to browse on the server
        return $ OutputFile $ joinPath $ htmlDir : filter (not . all (== '.')) xs
    where
        str = templateStr . lstrPack
        tagOptions sel = concat [tag "option" ["selected=selected" | x `elem` sel] x | x <- completionTags store]
        params = map (second str)
            [("cdn",cdn)
            ,("home",home)
            ,("jquery",if null cdn then "plugin/jquery.js" else JQuery.url)
            ,("version",showVersion version ++ " " ++ showUTCTime "%Y-%m-%d %H:%M" spawned)]
        templateIndex = templateFile (htmlDir </> "index.html") `templateApply` params
        templateEmpty = templateFile (htmlDir </>  "welcome.html")
        templateHome = templateIndex `templateApply` [("tags",str $ tagOptions []),("body",templateEmpty),("title",str "Hoogle"),("search",str ""),("robots",str "index")]
        templateLog = templateFile (htmlDir </> "log.html") `templateApply` params


dedupeTake :: Ord k => Int -> (v -> k) -> [v] -> [[v]]
dedupeTake n key = f [] Map.empty
    where
        -- map is Map k [v]
        f res mp xs | Map.size mp >= n || null xs = map (reverse . (Map.!) mp) $ reverse res
        f res mp (x:xs) | Just vs <- Map.lookup k mp = f res (Map.insert k (x:vs) mp) xs
                        | otherwise = f (k:res) (Map.insert k [x] mp) xs
            where k = key x


showResults :: Bool -> [(String, String)] -> [Query] -> [[Target]] -> String
showResults local args query results = unlines $
    ["<h1>" ++ renderQuery query ++ "</h1>"
    ,"<ul id=left>"
    ,"<li><b>Packages</b></li>"] ++
    [tag_ "li" $ f cat val | (cat,val) <- itemCategories $ concat results, QueryScope True cat val `notElem` query] ++
    ["</ul>"] ++
    ["<p>No results found</p>" | null results] ++
    ["<div class=result>" ++
     "<div class=ans><a href=\"" ++ showURL local targetURL ++ "\">" ++ displayItem query targetItem ++ "</a></div>" ++
     "<div class=from>" ++ showFroms local is  ++ "</div>" ++
     "<div class=\"doc newline shut\">" ++ targetDocs ++ "</div>" ++
     "</div>"
    | is@(Target{..}:_) <- results]
    where
        add x = escapeHTML $ ("?" ++) $ intercalate "&" $ map (joinPair "=") $
            case break ((==) "hoogle" . fst) args of
                (a,[]) -> a ++ [("hoogle",x)]
                (a,(_,x1):b) -> a ++ [("hoogle",x1 ++ " " ++ x)] ++ b

        f cat val = "<a class='minus' href='" ++ add ("-" ++ cat ++ ":" ++ val) ++ "'></a>" ++
                    "<a class='plus' href='" ++ add (cat ++ ":" ++ val) ++ "'>" ++
                    (if cat == "package" then "" else cat ++ ":") ++ val ++ "</a>"


itemCategories :: [Target] -> [(String,String)]
itemCategories xs =
    [("is","exact")] ++
    [("is","package") | any ((==) "package" . targetType) xs] ++
    [("is","module")  | any ((==) "module"  . targetType) xs] ++
    nubOrd [("package",p) | Just (p,_) <- map targetPackage xs]

showFroms :: Bool -> [Target] -> String
showFroms local xs = intercalate ", " $ for pkgs $ \p ->
    let ms = filter ((==) p . targetPackage) xs
    in unwords ["<a href=\"" ++ showURL local b ++ "\">" ++ a ++ "</a>" | (a,b) <- catMaybes $ p : map remod ms]
    where
        remod Target{..} = do (a,_) <- targetModule; return (a,targetURL)
        pkgs = nubOrd $ map targetPackage xs

showURL :: Bool -> URL -> String
showURL True (stripPrefix "file:///" -> Just x) = "file/" ++ x
showURL _ x = x


-------------------------------------------------------------
-- DISPLAY AN ITEM (bold keywords etc)

highlightItem :: [Query] -> String -> String
highlightItem qs x
    | Just (pre,x) <- stripInfix "<0>" x, Just (name,post) <- stripInfix "</0>" x = pre ++ highlight (unescapeHTML name) ++ post
    | otherwise = x
    where
        highlight = concatMap (\xs@((b,_):_) -> let s = escapeHTML $ map snd xs in if b then "<b>" ++ s ++ "</b>" else s) .
                    groupOn fst . (\x -> zip (f x) x)
            where
              f (x:xs) | m > 0 = replicate m True ++ drop (m - 1) (f xs)
                  where m = maximum $ 0 : [length y | QueryName y <- qs, lower y `isPrefixOf` lower (x:xs)]
              f (x:xs) = False : f xs
              f [] = []

displayItem :: [Query] -> String -> String
displayItem = highlightItem


action_server_test_ :: IO ()
action_server_test_ = do
    testing "Action.Server.displayItem" $ do
        let expand = replace "{" "<b>" . replace "}" "</b>" . replace "<0>" "" . replace "</0>" ""
            contract = replace "{" "" . replace "}" ""
        let q === s | displayItem (parseQuery q) (contract s) == expand s = putChar '.'
                    | otherwise = error $ show (q,s,displayItem (parseQuery q) (contract s))
        "test" === "<0>my{Test}</0> :: Int -&gt; test"
        "new west" === "<0>{newest}_{new}</0> :: Int"
        "+*" === "(<0>{+*}&amp;</0>) :: Int"
        "+<" === "(<0>&gt;{+&lt;}</0>) :: Int"
        "foo" === "<i>data</i> <0>{Foo}d</0>"
        "foo" === "<i>type</i> <0>{Foo}d</0>"
        "foo" === "<i>type family</i> <0>{Foo}d</0>"
        "foo" === "<i>module</i> Foo.Bar.<0>F{Foo}</0>"
        "foo" === "<i>module</i> <0>{Foo}o</0>"

action_server_test :: Bool -> FilePath -> IO ()
action_server_test sample database = do
    testing "Action.Server.replyServer" $ withSearch database $ \store -> do
        log <- logNone
        dataDir <- getDataDir
        let q === want = do
                OutputString (lstrUnpack -> res) <- replyServer log False store "" "" (dataDir </> "html") "" (Input [] [("hoogle",q)])
                if want `isInfixOf` res then putChar '.' else fail $ "Bad substring: " ++ res
        if sample then
            "Wife" === "<b>type family</b>"
         else do
            "<>" === "<span class=name>(<b>&lt;&gt;</b>)</span>"
            "filt" === "<span class=name><b>filt</b>er</span>"
            "True" === "https://hackage.haskell.org/package/base/docs/Prelude.html#v:True"


-------------------------------------------------------------
-- ANALYSE THE LOG


displayLog :: [Summary] -> String
displayLog xs = "[" ++ intercalate "," (map f xs) ++ "]"
    where
        f Summary{..} = "{date:" ++ show (showGregorian summaryDate) ++
                        ",users:" ++ show summaryUsers ++ ",uses:" ++ show summaryUses ++
                        ",slowest:" ++ show summarySlowest ++ ",average:" ++ show summaryAverage ++
                        ",errors:" ++ show summaryErrors ++ "}"
