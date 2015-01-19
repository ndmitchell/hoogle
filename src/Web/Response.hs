{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Web.Response(response, ResponseArgs(..), responseArgs) where

import CmdLine.All
import Hoogle
import General.Base
import General.System
import General.Web
import Web.Page
import Data.Generics.Uniplate
#if __GLASGOW_HASKELL__ < 710
import System.Locale
#endif

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time.Clock
import Data.Time.Format
import Network.Wai
import Network.HTTP.Types(hContentType)
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Paths_hoogle(version)
import Data.Version(showVersion)


logFile = "log.txt"
version = showVersion Paths_hoogle.version


data ResponseArgs = ResponseArgs
    {updatedCss :: String
    ,updatedJs :: String
    ,templates :: Templates
    }

responseArgs = ResponseArgs version version defaultTemplates


response :: ResponseArgs -> CmdLine -> IO Response
response ResponseArgs{..} q = do
    logMessage q
    let response x ys = responseOK ((hContentType, fromString x) : ys) . fromString

    dbs <- unsafeInterleaveIO $ case queryParsed q of
        Left _ -> return mempty
        Right x -> fmap snd $ loadQueryDatabases (databases q) (fromRight $ queryParsed q)

    case web q of
        Just "suggest" -> fmap (response "application/json" []) $ runSuggest q
        Just "embed" -> return $ response "text/html" [hdr] $ runEmbed dbs q
            where hdr = (fromString "Access-Control-Allow-Origin", fromString "*")
        Just "ajax" -> return $ response "text/html" [] $ runQuery templates True dbs q
        Just "json" -> return $ responseOK [(hContentType, fromString "application/json")] $ runJson dbs q
        Just "web" -> return $ response "text/html" [] $
            header templates updatedCss updatedJs (queryText q) ['-' | queryText q /= ""] ++
            runQuery templates False dbs q ++ footer templates version
        mode -> return $ response "text/html" [] $ "Unknown webmode: " ++ fromMaybe "none" mode


logMessage :: CmdLine -> IO ()
logMessage q = do
    time <- getCurrentTime
    args <- fmap (fromMaybe [("hoogle",queryText q)]) cgiArgs
    ip <- fmap (fromMaybe "0") $ getEnvVar "REMOTE_ADDR"
    let shw x = if all isAlphaNum x then x else show x
    appendFile logFile $ (++ "\n") $ unwords $
        [formatTime defaultTimeLocale "%FT%T" time
        ,ip] ++
        [shw a ++ "=" ++ shw b | (a,b) <- args]


runSuggest :: CmdLine -> IO String
runSuggest cq@Search{queryText=q} = do
    (_, db) <- loadQueryDatabases (databases cq) mempty
    let res = completions db q
    return $ "[" ++ show q ++ "," ++ show res ++ "]"
runSuggest _ = return ""


runEmbed :: Database -> CmdLine -> String
runEmbed dbs Search{queryParsed = Left err} = "<i>Parse error: " ++& errorMessage err ++ "</i>"
runEmbed dbs cq@Search{queryParsed = Right q}
    | null now = "<i>No results found</i>"
    | otherwise = unlines
        ["<a href='" ++ url ++ "'>" ++ showTagHTML (transform f $ self $ snd x) ++ "</a>"
        | x <- now, let url = fromList "" $ map fst $ locations $ snd x]
    where
        now = take (maybe 10 (max 1) $ count cq) $ search dbs q
        f (TagEmph x) = TagBold x
        f (TagBold x) = x
        f x = x


runJson :: Database -> CmdLine -> LBS.ByteString
runJson dbs Search{queryParsed = Left err} =
    J.encode $ J.object [ fromString "version"    J..= version
                        , fromString "parseError" J..= show err
                        ]
runJson dbs cq@Search{queryParsed = Right q} =
    J.encode $ J.object [ fromString "version"    J..= version
                        , fromString "results"    J..= results
                        ]
    where
        results | q == mempty = []
                | otherwise   = now

        start2 = maybe 0 (subtract 1 . max 0) $ start cq
        count2 = maybe 20 (max 1) $ count cq
        now = map (f . snd) $ take count2 $ drop start2 $ search dbs q

        f Result{..} = J.object
                       [ fromString "location" J..= (head $ map fst locations ++ [""])
                       , fromString "self" J..= showTagText self
                       , fromString "docs" J..= showTagText docs
                       ]


runQuery :: Templates -> Bool -> Database -> CmdLine -> String
runQuery templates ajax dbs Search{queryParsed = Left err} =
    parseError templates (showTagHTMLWith f $ parseInput err) (errorMessage err)
    where
        f (TagEmph x) = Just $ "<span class='error'>" ++ showTagHTMLWith f x ++ "</span>"
        f _ = Nothing


runQuery templates ajax dbs q | fromRight (queryParsed q) == mempty = welcome templates


runQuery templates ajax dbs cq@Search{queryParsed = Right q, queryText = qt} = unlines $
    (if prefix then
        ["<h1>" ++ qstr ++ "</h1>"] ++
        ["<ul id='left'><li><b>Packages</b></li>" ++ also ++ "</ul>" | not $ null pkgs] ++
        ["<p>" ++ showTag sug ++ "</p>" | Just sug <- [suggestions dbs q]] ++
        if null res then
            ["<p>No results found</p>"]
        else
            concat (pre ++ now)
     else
        concat now) ++
    ["<p><a href=\"" ++& urlMore ++ "\" class='more'>Show more results</a></p>" | not $ null post]
    where
        prefix = not $ ajax && start2 /= 0 -- show from the start, with header
        start2 = maybe 0 (subtract 1 . max 0) $ start cq
        count2 = maybe 20 (max 1) $ count cq

        src = search dbs q
        res = [renderRes i (i /= 0 && i == start2 && prefix) x | (i,(_,x)) <- zip [0..] src]
        (pre,res2) = splitAt start2 res
        (now,post) = splitAt count2 res2

        also = concatMap f (take (5 + length minus) $ nub $ minus ++ pkgs)
            where minus = [x | (False,x) <- queryPackages q]
        f x | (True,lx) `elem` queryPackages q =
                let q2 = showTagText $ renderQuery $ querySetPackage Nothing lx q in
                "<li><a class='minus' href='" ++ searchLink q2 ++ "'>" ++ x ++ "</a></li>"
            | (False,lx) `elem` queryPackages q =
                let q2 = showTagText $ renderQuery $ querySetPackage Nothing lx q in
                "<li><a class='plus pad' href='" ++ searchLink q2 ++ "'>" ++ x ++ "</a></li>"
            | otherwise =
                let link b = searchLink $ showTagText $ renderQuery $ querySetPackage (Just b) lx q in
                "<li><a class='minus' href='" ++ link False ++ "'></a>" ++
                "<a class='plus' href='" ++ link True ++ "'>" ++ x ++ "</a></li>"
            where lx = map toLower x
        pkgs = [x | (_, (_,x):_)  <- concatMap (locations . snd) $ take (start2+count2) src]

        urlMore = searchLink qt ++ "&start=" ++ show (start2+count2+1) ++ "#more"
        qstr = showTagHTML (renderQuery q)


renderRes :: Int -> Bool -> Result -> [String]
renderRes i more Result{..} =
        ["<a name='more'></a>" | more] ++
        ["<div class='result'>"] ++
        ["<div class='ans'>" ++ href selfUrl (showTagHTMLWith url self) ++ "</div>"] ++
        ["<div class='from'>" ++ intercalate ", " [unwords $ zipWith (f u) [1..] ps | (u,ps) <- locations] ++ "</div>" | not $ null locations] ++
        ["<div class='doc " ++ (if '\n' `elem` s then " newline" else "") ++ "'><span>" ++ showTag docs ++ "</span></div>"
            | let s = showTagText docs, s /= ""] ++
        ["</div>"]
    where
        selfUrl = head $ map fst locations ++ [""]
        f u cls (url,text) = "<a class='p" ++ show cls ++ "' href='" ++  url2 ++ "'>" ++ text ++ "</a>"
            where url2 = if url == takeWhile (/= '#') u then u else url

        url (TagBold x)
            | null selfUrl = Just $ "<span class='a'>" ++ showTagHTML (transform g x) ++ "</span>"
            | otherwise = Just $ "</a><a class='a' href='" ++& selfUrl ++ "'>" ++ showTagHTML (transform g x) ++
                                 "</a><a class='dull' href='" ++& selfUrl ++ "'>"
        url _ = Nothing

        g (TagEmph x) = TagBold x
        g x = x

        href url x = if null url then x else "<a class='dull' href='" ++& url ++ "'>" ++ x ++ "</a>"


showTag :: TagStr -> String
showTag = showTagHTML . transform f
    where
        f (TagLink "" x) = TagLink (if any (`isPrefixOf` str) ["http:","https:"] then str else searchLink str) x
            where str = showTagText x
        f x = x


searchLink :: String -> URL
searchLink x = "?hoogle=" ++% x
