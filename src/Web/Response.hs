{-# LANGUAGE RecordWildCards #-}

module Web.Response(response) where

import CmdLine.All
import Hoogle
import General.Code
import Web.Page
import Web.Text
import Data.Generics.Uniplate

import Data.Monoid
import Data.Time.Clock
import Data.Time.Calendar
import General.Web hiding (escapeHTML)
import Paths_hoogle


logFile = "log.txt"


response :: FilePath -> CmdLine -> IO ([Header], String)
response resources q = do
    print q
    logMessage q
    (typ,res) <-
        if webmode q == Just "suggest" then do
            fmap ((,) "application/json") $ runSuggest q
        else do
            dbs <- if isRight $ queryParsed q
                   then fmap snd $ loadQueryDatabases (databases q) (fromRight $ queryParsed q)
                   else return mempty
            return $ (,) "text/html" $ unlines $ header resources (escapeHTML $ queryText q) ++ runQuery dbs q ++ footer
    {-
    when (Debug `elem` queryFlags q) $
        writeFile "temp.htm" res
    sequence_ [writeFile x res | Output x <- queryFlags q]
    -}
    return ([headerContentType typ], res)


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


-- TODO: Should escape the query text
runQuery :: Database -> CmdLine -> [String]
runQuery dbs Search{queryText = text, queryParsed = Left (ParseError _ pos txt)} =
    ["<h1><b>Parse error in user query</b></h1>"
    ,"<p>"
    ,"  Query: <tt>" +& pre ++ "<span id='error'>" +& post2 ++ "</span></tt><br/>"
    ,"</p><p>"
    ,"  Error: " +& txt ++ "<br/>"
    ,"</p><p>"
    ,"  For information on what queries should look like, see the"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>user manual</a>."
    ,"</p>"
    ]
    where
        (pre,post) = splitAt pos text
        post2 = if null post then concat (replicate 3 "&nbsp;") else post


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

        moreResults = "<tr><td></td><td><a href=\"" +& urlMore ++ "\" class='more'>Show more results</a></td></tr>"
        urlMore = "?hoogle=" +% queryText cq ++ "&start=" ++ show (start2+count2+1) ++ "#more"

        qstr = showTagHTML (renderQuery q)
        qurl (TagHyperlink url x) | "query:" `isPrefixOf` url = TagHyperlink ("?hoogle=" +% drop 6 url) x
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
        [tr $ td "mod" (f package) ++ td "ans" (href selfUrl $ showTagHTMLWith url selfText)
        ,tr $ td "pkg" (f modul) ++ td "doc" docs2]
    where
        (selfUrl,selfText) = self
        f = maybe "" (uncurry href)

        docs2 = ("<div id='d" ++ show i ++ "' class='shut'>" ++
                   "<a class='docs' onclick='return docs(" ++ show i ++ ")' href='" +& selfUrl ++ "'></a>") +?
                   (showTagHTML selfText) +?
               "</div>"

        url (TagHyperlink _ x)
            | null selfUrl = Just $ "<span class='a'>" ++ showTagHTML x ++ "</span>"
            | otherwise = Just $ "</a><a href='" +& selfUrl ++ "'>" ++ showTagHTML x ++
                                 "</a><a class='dull' href='" +& selfUrl ++ "'>"
        url _ = Nothing

tr x = "<tr>" ++ x ++ "</tr>"
td c x = "<td" ++ (if null c then "" else " class='" ++ c ++ "'") ++ ">" ++ x ++ "</td>"
href url x = if null url then x else "<a class='dull' href='" +& url ++ "'>" ++ x ++ "</a>"
