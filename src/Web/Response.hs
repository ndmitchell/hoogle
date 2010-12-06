{-# LANGUAGE RecordWildCards #-}

module Web.Response(response) where

import CmdLine.All
import Hoogle
import General.Base
import General.Web
import Web.Page
import Data.Generics.Uniplate

import Data.Time.Clock
import Data.Time.Calendar
import Network.HTTP
import Paths_hoogle


logFile = "log.txt"


response :: FilePath -> CmdLine -> IO (Response String)
response resources q = do
    logMessage q
    let r200 x = Response (2,0,0) "OK" [Header HdrContentType x]
    case webmode q of
        Just "suggest" -> fmap (r200 "application/json") $ runSuggest q
        Just "ajax" -> do
            dbs <- if isRight $ queryParsed q
                   then fmap snd $ loadQueryDatabases (databases q) (fromRight $ queryParsed q)
                   else return mempty
            return $ r200 "text/html" $ unlines $ runQuery dbs q
        Nothing -> do
            dbs <- if isRight $ queryParsed q
                   then fmap snd $ loadQueryDatabases (databases q) (fromRight $ queryParsed q)
                   else return mempty
            return $ r200 "text/html" $ unlines $ header resources (escapeHTML $ queryText q) ++ runQuery dbs q ++ footer
        Just e -> return $ r200 "text/html" $ "Unknown webmode: " ++ show e


logMessage :: CmdLine -> IO ()
logMessage q = do
    time <- getCurrentTime
    cgi <- liftM (fromMaybe []) cgiArgs
    appendFile logFile $ (++ "\n") $ unwords $
        [showGregorian (utctDay time)
        ,show (queryText q)] ++
        ["?" ++ a ++ "=" ++ c ++ b ++ c | (a,b) <- cgi, let c = ['\"' | any isSpace b]]


runSuggest :: CmdLine -> IO String
runSuggest Search{queryText=q} = do
    root <- getDataDir
    db <- loadDatabase $ root </> "default.hoo"
    let res = queryCompletions db q
    return $ "[" ++ show q ++ "," ++ show res ++ "]"
runSuggest _ = return ""



runQuery :: Database -> CmdLine -> [String]
runQuery dbs Search{queryParsed = Left err} =
    ["<h1><b>Parse error in user query</b></h1>"
    ,"<p>"
    ,"  Query: <span id='error'>" ++ showTagHTMLWith f (parseInput err) ++ "</span><br/>"
    ,"</p><p>"
    ,"  Error: " ++& errorMessage err ++ "<br/>"
    ,"</p><p>"
    ,"  For information on what queries should look like, see the"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>user manual</a>."
    ,"</p>"
    ]
    where
        f (TagEmph x) = Just $ "<u>" ++ showTagHTMLWith f x ++ "</u>"
        f _ = Nothing


runQuery dbs q | isBlankQuery $ fromRight $ queryParsed q = welcome


runQuery dbs cq@Search{queryParsed = Right q} =
    ["<h1>Searching for " ++ qstr ++ "</h1>"] ++
    ["<p>" ++ showTagHTML (transform qurl sug) ++ "</p>" | Just sug <- [querySuggestions dbs q]] ++
    if null res then
        ["<p>No results found</p>"]
    else
        ["<table>"] ++
        concatMap (uncurry renderRes) pre ++
        insertMore (concatMap (uncurry renderRes) now) ++
        [moreResults | not $ null post] ++
        ["</table>"]
    where
        start2 = maybe 0 (subtract 1 . max 0) $ start cq
        count2 = maybe 20 (max 1) $ count cq
        res = zip [0..] $ map snd $ searchRange (start2,start2+count2) dbs q
        (pre,res2) = splitAt start2 res
        (now,post) = splitAt count2 res2

        moreResults = "<tr><td></td><td><a href=\"" ++& urlMore ++ "\" class='more'>Show more results</a></td></tr>"
        urlMore = "?hoogle=" ++% queryText cq ++ "&start=" ++ show (start2+count2+1) ++ "#more"

        qstr = showTagHTML (renderQuery q)
        qurl (TagLink url x) | "query:" `isPrefixOf` url = TagLink ("?hoogle=" ++% drop 6 url) x
        qurl x = x



-- insert <a name=more> where you can
insertMore :: [String] -> [String]
insertMore [] = []
insertMore (x:xs) = f x : xs
    where
        f ('>':xs) | not $ "<td" `isPrefixOf` xs = "><a name='more'></a>" ++ xs
        f (x:xs) = x : f xs
        f [] = []


renderRes :: Int -> Result -> [String]
renderRes i Result{..} =
        [tr $ td "mod" (f modul) ++ td "ans" (href selfUrl $ showTagHTMLWith url selfText)
        ,tr $ td "pkg" (f package) ++ td "doc" docs2]
    where
        (selfUrl,selfText) = self
        f = maybe "" (uncurry href)

        docs2 = ("<div id='d" ++ show i ++ "' class='shut'>" ++
                   "<a class='docs' onclick='return docs(" ++ show i ++ ")' href='" ++& selfUrl ++ "'></a>") ++?
                   showTagHTML docs ++?
               "</div>"

        url (TagBold x)
            | null selfUrl = Just $ "<span class='a'>" ++ showTagHTML (transform g x) ++ "</span>"
            | otherwise = Just $ "</a><a class='a' href='" ++& selfUrl ++ "'>" ++ showTagHTML (transform g x) ++
                                 "</a><a class='dull' href='" ++& selfUrl ++ "'>"
        url _ = Nothing

        g (TagEmph x) = TagBold x
        g x = x


tr x = "<tr>" ++ x ++ "</tr>"
td c x = "<td" ++ (if null c then "" else " class='" ++ c ++ "'") ++ ">" ++ x ++ "</td>"
href url x = if null url then x else "<a class='dull' href='" ++& url ++ "'>" ++ x ++ "</a>"
