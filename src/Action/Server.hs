{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Server(actionServer, actionReplay, action_server_test_, action_server_test) where

import Data.List.Extra
import System.FilePath
import Control.Exception
import Control.DeepSeq
import System.Directory
import Data.Tuple.Extra
import qualified Language.Javascript.JQuery as JQuery
import qualified Language.Javascript.Flot as Flot
import Data.Version
import Paths_hoogle
import Data.Maybe
import Control.Monad
import Text.Read
import System.IO.Extra
import General.Str
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import System.Time.Extra
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Unsafe
import Numeric.Extra
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
        \x -> BS.pack "hoogle=" `BS.isInfixOf` x && not (BS.pack "is:ping" `BS.isInfixOf` x)
    putStrLn . showDuration =<< time
    evaluate spawned
    dataDir <- case datadir of
        Just d -> return d
        Nothing -> getDataDir
    haddock <- maybe (return Nothing) (fmap Just . canonicalizePath) haddock
    withSearch database $ \store ->
        server log cmd $ replyServer log local links haddock store cdn home (dataDir </> "html") scope

actionReplay :: CmdLine -> IO ()
actionReplay Replay{..} = withBuffering stdout NoBuffering $ do
    src <- readFile logs
    let qs = [readInput url | _:ip:_:url:_ <- map words $ lines src, ip /= "-"]
    (t,_) <- duration $ withSearch database $ \store -> do
        log <- logNone
        dataDir <- getDataDir
        let op = replyServer log False False Nothing store "" "" (dataDir </> "html") scope
        replicateM_ repeat_ $ forM_ qs $ \x -> do
            res <- op x
            evaluate $ rnf res
            putChar '.'
    putStrLn $ "\nTook " ++ showDuration t ++ " (" ++ showDuration (t / intToDouble (repeat_ * length qs)) ++ ")"

{-# NOINLINE spawned #-}
spawned :: UTCTime
spawned = unsafePerformIO getCurrentTime

replyServer :: Log -> Bool -> Bool -> Maybe FilePath -> StoreRead -> String -> String -> FilePath -> String -> Input -> IO Output
replyServer log local links haddock store cdn home htmlDir scope Input{..} = case inputURL of
    -- without -fno-state-hack things can get folded under this lambda
    [] -> do
        let grab name = [x | (a,x) <- inputArgs, a == name, x /= ""]
        let qScope = let xs = grab "scope" in [scope | null xs && scope /= ""] ++ xs
        let qSource = grab "hoogle" ++ filter (/= "set:stackage") qScope
        let q = concatMap parseQuery qSource
        let (q2, results) = search store q
        let body = showResults local links haddock (filter ((/= "mode") . fst) inputArgs) q2 $
                dedupeTake 25 (\t -> t{targetURL="",targetPackage=Nothing, targetModule=Nothing}) results
        case lookup "mode" $ reverse inputArgs of
            Nothing | qSource /= [] -> fmap OutputHTML $ templateRender templateIndex $ map (second str)
                        [("tags",tagOptions qScope)
                        ,("body",body)
                        ,("title",unwords qSource ++ " - Hoogle")
                        ,("search",unwords $ grab "hoogle")
                        ,("robots",if any isQueryScope q then "none" else "index")]
                    | otherwise -> OutputHTML <$> templateRender templateHome []
            Just "body" -> OutputHTML <$> if null qSource then templateRender templateEmpty [] else return $ lbstrPack body
            Just "json" ->
              let argRead :: Read a => String -> a -> a
                  argRead key def = fromMaybe def $
                    readMaybe =<< lookup key inputArgs
                  -- 1 means don't drop anything, if it's less than 1 ignore it
                  start :: Int
                  start = max 0 $ (argRead "start" 1) - 1
                  -- by default it returns 100 entries
                  count :: Int
                  count = min 500 $ argRead "count" 100
              in pure $ OutputJSON $ JSON.encode $ take count $ drop start results
            Just m -> return $ OutputFail $ lbstrPack $ "Mode " ++ m ++ " not (currently) supported"
    ["plugin","jquery.js"] -> OutputFile <$> JQuery.file
    ["plugin","jquery.flot.js"] -> OutputFile <$> Flot.file Flot.Flot
    ["plugin","jquery.flot.time.js"] -> OutputFile <$> Flot.file Flot.FlotTime

    ["canary"] -> do
        now <- getCurrentTime
        summ <- logSummary log
        let errs = sum [summaryErrors | Summary{..} <- summ, summaryDate >= pred (utctDay now)]
        let alive = fromRational $ toRational $ (now `diffUTCTime` spawned) / (24 * 60 * 60)
        return $ (if errs == 0 && alive < 1.5 then OutputText else OutputFail) $ lbstrPack $
            "Errors " ++ (if errs == 0 then "good" else "bad") ++ ": " ++ show errs ++ " in the last 24 hours.\n" ++
            "Updates " ++ (if alive < 1.5 then "good" else "bad") ++ ": Last updated " ++ showDP 2 alive ++ " days ago.\n"

    ["log"] -> do
        log <- displayLog <$> logSummary log
        OutputHTML <$> templateRender templateLog [("data",str log)]
    ["stats"] -> do
        stats <- getStatsDebug
        return $ case stats of
            Nothing -> OutputFail $ lbstrPack "GHC Statistics is not enabled, restart with +RTS -T"
            Just x -> OutputText $ lbstrPack $ replace ", " "\n" $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') $ show x
    "haddock":xs | Just x <- haddock -> do
        let file = intercalate "/" $ filter (not . (== "..")) (x:xs)
        return $ OutputFile $ file ++ (if hasTrailingPathSeparator file then "index.html" else "")
    "file":xs | local -> do
        let x = ['/' | not isWindows] ++ intercalate "/" (dropWhile null xs)
        let file = x ++ (if hasTrailingPathSeparator x then "index.html" else "")
        if takeExtension file /= ".html" then
            return $ OutputFile file
         else do
            src <- readFile file
            -- Haddock incorrectly generates file:// on Windows, when it should be file:///
            -- so replace on file:// and drop all leading empty paths above
            return $ OutputHTML $ lbstrPack $ replace "file://" "/file/" src
    xs ->
        -- avoid "" and ".." in the URLs, since they could be trying to browse on the server
        return $ OutputFile $ joinPath $ htmlDir : filter (not . all (== '.')) xs
    where
        str = templateStr . lbstrPack
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


showResults :: Bool -> Bool -> Maybe FilePath -> [(String, String)] -> [Query] -> [[Target]] -> String
showResults local links haddock args query results = unlines $
    ["<h1>" ++ renderQuery query ++ "</h1>"
    ,"<ul id=left>"
    ,"<li><b>Packages</b></li>"] ++
    [tag_ "li" $ f cat val | (cat,val) <- itemCategories $ concat results, QueryScope True cat val `notElem` query] ++
    ["</ul>"] ++
    ["<p>No results found</p>" | null results] ++
    ["<div class=result>" ++
     "<div class=ans>" ++
        "<a href=\"" ++ showURL local haddock targetURL ++ "\">" ++ displayItem query targetItem ++ "</a>" ++
        (if not links then "" else "<div class=links><a href='" ++ useLink is ++ "'>Uses</a></div>") ++
     "</div>" ++
     "<div class=from>" ++ showFroms local haddock is ++ "</div>" ++
     "<div class=\"doc newline shut\">" ++ targetDocs ++ "</div>" ++
     "</div>"
    | is@(Target{..}:_) <- results]
    where
        useLink ts@(t:_)=
            "https://codesearch.aelve.com/haskell/search?query=" ++ escapeURL (extractName $ targetItem t) ++
            "&filter=" ++ intercalate "|" (mapMaybe (fmap fst . targetModule) ts) ++
            "&precise=on"

        add x = escapeHTML $ ("?" ++) $ intercalate "&" $ map (joinPair "=") $
            case break ((==) "hoogle" . fst) args of
                (a,[]) -> a ++ [("hoogle",x)]
                (a,(_,x1):b) -> a ++ [("hoogle",x1 ++ " " ++ x)] ++ b

        f cat val = "<a class=\"minus\" href=\"" ++ add ("-" ++ cat ++ ":" ++ val) ++ "\"></a>" ++
                    "<a class=\"plus\" href=\"" ++ add (cat ++ ":" ++ val) ++ "\">" ++
                    (if cat == "package" then "" else cat ++ ":") ++ val ++ "</a>"


-- find the <span class=name>X</span> bit
extractName :: String -> String
extractName x
    | Just (_, x) <- stripInfix "<span class=name>" x
    , Just (x, _) <- stripInfix "</span>" x
    = unHTML x
extractName x = x


itemCategories :: [Target] -> [(String,String)]
itemCategories xs =
    [("is","exact")] ++
    [("is","package") | any ((==) "package" . targetType) xs] ++
    [("is","module")  | any ((==) "module"  . targetType) xs] ++
    nubOrd [("package",p) | Just (p,_) <- map targetPackage xs]

showFroms :: Bool -> Maybe FilePath -> [Target] -> String
showFroms local haddock xs = intercalate ", " $ flip map pkgs $ \p ->
    let ms = filter ((==) p . targetPackage) xs
    in unwords ["<a href=\"" ++ showURL local haddock b ++ "\">" ++ a ++ "</a>" | (a,b) <- catMaybes $ p : map remod ms]
    where
        remod Target{..} = do (a,_) <- targetModule; return (a,targetURL)
        pkgs = nubOrd $ map targetPackage xs

showURL :: Bool -> Maybe FilePath -> URL -> String
showURL _ (Just _) x = "haddock" ++ x
showURL True _ (stripPrefix "file:///" -> Just x) = "file/" ++ x
showURL _ _ x = x


-------------------------------------------------------------
-- DISPLAY AN ITEM (bold keywords etc)

highlightItem :: [Query] -> String -> String
highlightItem qs x
    | Just (pre,x) <- stripInfix "<s0>" x, Just (name,post) <- stripInfix "</s0>" x = pre ++ highlight (unescapeHTML name) ++ post
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
        let expand = replace "{" "<b>" . replace "}" "</b>" . replace "<s0>" "" . replace "</s0>" ""
            contract = replace "{" "" . replace "}" ""
        let q === s | displayItem (parseQuery q) (contract s) == expand s = putChar '.'
                    | otherwise = error $ show (q,s,displayItem (parseQuery q) (contract s))
        "test" === "<s0>my{Test}</s0> :: Int -&gt; test"
        "new west" === "<s0>{newest}_{new}</s0> :: Int"
        "+*" === "(<s0>{+*}&amp;</s0>) :: Int"
        "+<" === "(<s0>&gt;{+&lt;}</s0>) :: Int"
        "foo" === "<i>data</i> <s0>{Foo}d</s0>"
        "foo" === "<i>type</i> <s0>{Foo}d</s0>"
        "foo" === "<i>type family</i> <s0>{Foo}d</s0>"
        "foo" === "<i>module</i> Foo.Bar.<s0>F{Foo}</s0>"
        "foo" === "<i>module</i> <s0>{Foo}o</s0>"

action_server_test :: Bool -> FilePath -> IO ()
action_server_test sample database = do
    testing "Action.Server.replyServer" $ withSearch database $ \store -> do
        log <- logNone
        dataDir <- getDataDir
        let q === want = do
                OutputHTML (lbstrUnpack -> res) <- replyServer log False False Nothing store "" "" (dataDir </> "html") "" (Input [] [("hoogle",q)])
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
                        ",slowest:" ++ show summarySlowest ++ ",average:" ++ show (fromAverage summaryAverage) ++
                        ",errors:" ++ show summaryErrors ++ "}"
