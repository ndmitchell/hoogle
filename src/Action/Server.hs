{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Server(actionServer, actionReplay, action_server_test_, action_server_test) where

import Data.List.Extra
import System.FilePath
import Control.Exception
import Control.Exception.Extra
import Control.DeepSeq
import System.Directory
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as H
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
import qualified Data.ByteString.Lazy.Char8 as LBS
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
import Data.Monoid
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
            grabInt name def = fromMaybe def $ readMaybe =<< listToMaybe (grab name) :: Int

        let qScope = let xs = grab "scope" in [scope | null xs && scope /= ""] ++ xs
        let qSource = grab "hoogle" ++ filter (/= "set:stackage") qScope
        let q = concatMap parseQuery qSource
        let (q2, results) = search store q
        let body = showResults local links haddock (filter ((/= "mode") . fst) inputArgs) q2 $
                dedupeTake 25 (\t -> t{targetURL="",targetPackage=Nothing, targetModule=Nothing}) results
        case lookup "mode" inputArgs of
            Nothing | qSource /= [] -> fmap OutputHTML $ templateRender templateIndex
                        [("tags", html $ tagOptions qScope)
                        ,("body", html body)
                        ,("title", text $ unwords qSource ++ " - Hoogle")
                        ,("search", text $ unwords $ grab "hoogle")
                        ,("robots", text $ if any isQueryScope q then "none" else "index")]
                    | otherwise -> OutputHTML <$> templateRender templateHome []
            Just "body" -> OutputHTML <$> if null qSource then templateRender templateEmpty [] else templateRender (html body) []
            Just "json" ->
              let -- 1 means don't drop anything, if it's less than 1 ignore it
                  start :: Int
                  start = max 0 $ grabInt "start" 1 - 1
                  -- by default it returns 100 entries
                  count :: Int
                  count = min 500 $ grabInt "count" 100
              in pure $ OutputJSON $ JSON.toEncoding $ take count $ drop start results
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
        OutputHTML <$> templateRender templateLog [("data",html $ H.string log)]
    ["stats"] -> do
        stats <- getStatsDebug
        return $ case stats of
            Nothing -> OutputFail $ lbstrPack "GHC Statistics is not enabled, restart with +RTS -T"
            Just x -> OutputText $ lbstrPack $ replace ", " "\n" $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') $ show x
    "haddock":xs | Just x <- haddock -> do
        let file = intercalate "/" $ x:xs
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
        return $ OutputFile $ joinPath $ htmlDir : xs
    where
        html = templateMarkup
        text = templateMarkup . H.string

        tagOptions sel = mconcat [H.option !? (x `elem` sel, H.selected "selected") $ H.string x | x <- completionTags store]
        params =
            [("cdn", text cdn)
            ,("home", text home)
            ,("jquery", text $ if null cdn then "plugin/jquery.js" else JQuery.url)
            ,("version", text $ showVersion version ++ " " ++ showUTCTime "%Y-%m-%d %H:%M" spawned)]
        templateIndex = templateFile (htmlDir </> "index.html") `templateApply` params
        templateEmpty = templateFile (htmlDir </>  "welcome.html")
        templateHome = templateIndex `templateApply` [("tags",html $ tagOptions []),("body",templateEmpty),("title",text "Hoogle"),("search",text ""),("robots",text "index")]
        templateLog = templateFile (htmlDir </> "log.html") `templateApply` params


dedupeTake :: Ord k => Int -> (v -> k) -> [v] -> [[v]]
dedupeTake n key = f [] Map.empty
    where
        -- map is Map k [v]
        f res mp xs | Map.size mp >= n || null xs = map (reverse . (Map.!) mp) $ reverse res
        f res mp (x:xs) | Just vs <- Map.lookup k mp = f res (Map.insert k (x:vs) mp) xs
                        | otherwise = f (k:res) (Map.insert k [x] mp) xs
            where k = key x


showResults :: Bool -> Bool -> Maybe FilePath -> [(String, String)] -> [Query] -> [[Target]] -> Markup
showResults local links haddock args query results = do
    H.h1 $ renderQuery query
    H.ul ! H.id "left" $ do
        H.li $ H.b "Packages"
        mconcat [H.li $ f cat val | (cat,val) <- itemCategories $ concat results, QueryScope True cat val `notElem` query]
    when (null results) $ H.p "No results found"
    forM_ results $ \is@(Target{..}:_) -> do
        H.div ! H.class_ "result" $ do
            H.div ! H.class_ "ans" $ do
                H.a ! H.href (H.stringValue $ showURL local haddock targetURL) $
                    displayItem query targetItem
                when links $ do
                    H.div ! H.class_ "links" $ H.a ! H.href (H.stringValue $ useLink is) $ "Uses"
            H.div ! H.class_ "from" $ showFroms local haddock is
            H.div ! H.class_ "doc newline shut" $ H.preEscapedString targetDocs
    where
        useLink :: [Target] -> String
        useLink [t] | isNothing $ targetPackage t =
            "https://packdeps.haskellers.com/reverse/" ++ extractName (targetItem t)
        useLink ts@(t:_) =
            "https://codesearch.aelve.com/haskell/search?query=" ++ escapeURL (extractName $ targetItem t) ++
            "&filter=" ++ intercalate "|" (mapMaybe (fmap fst . targetModule) ts) ++
            "&precise=on"

        add x = ("?" ++) $ intercalate "&amp;" $ map (joinPair "=") $
            case break ((==) "hoogle" . fst) args of
                (a,[]) -> a ++ [("hoogle", x)]
                (a,(_,x1):b) -> a ++ [("hoogle", x1 ++ " " ++ x)] ++ b

        f cat val = do
            H.a ! H.class_" minus" ! H.href (H.stringValue $ add $ "-" ++ cat ++ ":" ++ val) $ ""
            H.a ! H.class_ "plus"  ! H.href (H.stringValue $ add $        cat ++ ":" ++ val) $
                H.string $ (if cat == "package" then "" else cat ++ ":") ++ val


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

showFroms :: Bool -> Maybe FilePath -> [Target] -> Markup
showFroms local haddock xs = mconcat $ intersperse ", " $ flip map pkgs $ \p ->
    let ms = filter ((==) p . targetPackage) xs
    in mconcat [H.a ! H.href (H.stringValue $ showURL local haddock b) $ H.string a | (a,b) <- catMaybes $ p : map remod ms]
    where
        remod Target{..} = do (a,_) <- targetModule; return (a,targetURL)
        pkgs = nubOrd $ map targetPackage xs

showURL :: Bool -> Maybe FilePath -> URL -> String
showURL _ (Just _) x = "haddock" ++ x
showURL True _ (stripPrefix "file:///" -> Just x) = "file/" ++ x
showURL _ _ x = x


-------------------------------------------------------------
-- DISPLAY AN ITEM (bold keywords etc)

highlightItem :: [Query] -> String -> Markup
highlightItem qs x
    | Just (pre,x) <- stripInfix "<s0>" x, Just (name,post) <- stripInfix "</s0>" x
        = H.preEscapedString pre <> highlight (unescapeHTML name) <> H.preEscapedString post
    | otherwise = H.string x
    where
        highlight = mconcat . map (\xs@((b,_):_) -> let s = H.string $ map snd xs in if b then H.b s else s) .
                    groupOn fst . (\x -> zip (f x) x)
            where
              f (x:xs) | m > 0 = replicate m True ++ drop (m - 1) (f xs)
                  where m = maximum $ 0 : [length y | QueryName y <- qs, lower y `isPrefixOf` lower (x:xs)]
              f (x:xs) = False : f xs
              f [] = []

displayItem :: [Query] -> String -> Markup
displayItem = highlightItem


action_server_test_ :: IO ()
action_server_test_ = do
    testing "Action.Server.displayItem" $ do
        let expand = replace "{" "<b>" . replace "}" "</b>" . replace "<s0>" "" . replace "</s0>" ""
            contract = replace "{" "" . replace "}" ""
        let q === s | LBS.unpack (renderMarkup $ displayItem (parseQuery q) (contract s)) == expand s = putChar '.'
                    | otherwise = errorIO $ show (q,s,renderMarkup $ displayItem (parseQuery q) (contract s))
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
        let check p q = do
                OutputHTML (lbstrUnpack -> res) <- replyServer log False False Nothing store "" "" (dataDir </> "html") "" (Input [] [("hoogle",q)])
                if p res then putChar '.' else fail $ "Bad substring: " ++ res
        let q === want = check (want `isInfixOf`) q
        let q /== want = check (not . isInfixOf want) q
        "<test" /== "<test"
        "&test" /== "&test"
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
