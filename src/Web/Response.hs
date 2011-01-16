{-# LANGUAGE RecordWildCards #-}

module Web.Response(response) where

import CmdLine.All
import Hoogle
import General.Base
import General.Util
import General.Web
import Web.Page
import Data.Generics.Uniplate

import Data.Time.Clock
import Data.Time.Calendar
import Network.HTTP


logFile = "log.txt"


response :: FilePath -> CmdLine -> IO (Response String)
response resources q = do
    logMessage q
    let response x = responseOk [Header HdrContentType x]

    let res ajax = do
            dbs <- if isRight $ queryParsed q
                   then fmap snd $ loadQueryDatabases (databases q) (fromRight $ queryParsed q)
                   else return mempty
            return $ runQuery ajax dbs q

    case web q of
        Just "ajax" -> do
            res <- res True
            return $ response "text/html" $ unlines res
        Just "web" -> do
            res <- res False
            return $ response "text/html" $ unlines $ header resources (escapeHTML $ queryText q) ++ res ++ footer
        Just "suggest" -> fmap (response "application/json") $ runSuggest q
        Just e -> return $ response "text/html" $ "Unknown webmode: " ++ e


logMessage :: CmdLine -> IO ()
logMessage q = do
    time <- getCurrentTime
    cgi <- fmap (fromMaybe []) cgiArgs
    appendFile logFile $ (++ "\n") $ unwords $
        [showGregorian (utctDay time)
        ,show (queryText q)] ++
        ["?" ++ a ++ "=" ++ c ++ b ++ c | (a,b) <- cgi, let c = ['\"' | any isSpace b]]


runSuggest :: CmdLine -> IO String
runSuggest cq@Search{queryText=q} = do
    (_, db) <- loadQueryDatabases (databases cq) (Query [] Nothing [])
    let res = queryCompletions db q
    return $ "[" ++ show q ++ "," ++ show res ++ "]"
runSuggest _ = return ""


runQuery :: Bool -> Database -> CmdLine -> [String]
runQuery ajax dbs Search{queryParsed = Left err} =
    ["<h1>" ++ showTagHTMLWith f (parseInput err) ++ "</h1>"
    ,"<p>"
    ,"  <b>Parse error:</b> " ++& errorMessage err
    ,"</p><p>"
    ,"  For information on what queries should look like, see the"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>user manual</a>."
    ,"</p>"
    ]
    where
        f (TagEmph x) = Just $ "<span class='error'>" ++ showTagHTMLWith f x ++ "</span>"
        f _ = Nothing


runQuery ajax dbs q | isBlankQuery $ fromRight $ queryParsed q = welcome


runQuery ajax dbs cq@Search{queryParsed = Right q, queryText = qt} =
    (if prefix then
        ["<h1>" ++ qstr ++ "</h1>"] ++
        ["<div id='left'>" ++ also ++ "</div>" | not $ null pkgs] ++
        ["<p>" ++ showTag sug ++ "</p>" | Just sug <- [querySuggestions dbs q]] ++
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

        also = "<ul><li><b>Packages</b></li>" ++ concatMap f (take (5 + length minus) $ nub $ minus ++ pkgs) ++ "</ul>"
            where minus = [x | MinusPackage x <- scope q]
        f x | PlusPackage lx `elem` scope q =
                let q2 = showTagText $ renderQuery $ q{scope = filter (/= PlusPackage lx) $ scope q} in
                "<li><a class='minus' href='" ++ searchLink q2 ++ "'>" ++ x ++ "</a></li>"
            | MinusPackage lx `elem` scope q =
                let q2 = showTagText $ renderQuery $ q{scope = filter (/= MinusPackage lx) $ scope q} in
                "<li><a class='plus pad' href='" ++ searchLink q2 ++ "'>" ++ x ++ "</a></li>"
            | otherwise =
                "<li><a class='minus' href='" ++ searchLink (qt ++ " -" ++ lx) ++ "'></a>" ++
                "<a class='plus' href='" ++ searchLink (qt ++ " +" ++ lx) ++ "'>" ++ x ++ "</a></li>"
            where lx = map toLower x
        pkgs = [x | (_, (_,x):_)  <- concatMap (locations . snd) $ take (start2+count2) src]

        urlMore = searchLink qt ++ "&start=" ++ show (start2+count2+1) ++ "#more"
        qstr = showTagHTML (renderQuery q)


renderRes :: Int -> Bool -> Result -> [String]
renderRes i more Result{..} =
        ["<a name='more'></a>" | more] ++
        ["<div class='ans'>" ++ href selfUrl (showTagHTMLWith url self) ++ "</div>"] ++
        ["<div class='from'>" ++ intercalate ", " [unwords $ zipWith (f u) [1..] ps | (u,ps) <- locations] ++ "</div>" | not $ null locations] ++
        ["<div class='doc'>" ++ docs2 ++ "</div>" | showTagText docs /= ""]
    where
        selfUrl = head $ map fst locations ++ [""]
        f u cls (url,text) = "<a class='p" ++ show cls ++ "' href='" ++  url2 ++ "'>" ++ text ++ "</a>"
            where url2 = if url == takeWhile (/= '#') u then u else url

        docs2 = ("<div id='d" ++ show i ++ "' class='shut'>" ++
                   "<a class='docs' onclick='return docs(" ++ show i ++ ")' href='" ++& selfUrl ++ "'></a>") ++?
                   showTag docs ++?
               "</div>"

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
        f (TagLink "" x) = TagLink (if "http:" `isPrefixOf` str then str else searchLink str) x
            where str = showTagText x
        f x = x
