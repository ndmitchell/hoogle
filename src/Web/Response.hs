
module Web.Response(response) where

import CmdLine.All
import Hoogle.All
import Hoogle.Query.All
import Hoogle.Item.All
import Hoogle.Search.All
import General.Code
import System.IO.Unsafe(unsafeInterleaveIO)
import Web.Page
import Web.Text
import Data.TagStr
import Data.Range
import Data.Binary.Defer.Index
import Data.Generics.Uniplate

import Data.Time.Clock
import Data.Time.Calendar
import General.Web hiding (escapeHTML)
import Paths_hoogle


logFile = "log.txt"


response :: CmdLine -> IO ([Header], String)
response q = do
    print q
    logMessage q
    (typ,res) <-
        if webmode q == Just "suggest" then do
            fmap ((,) "application/json") $ runSuggest q
        else do
            (skipped,dbs) <- loadDataBases q
            return $ (,) "text/html" $ unlines $ header (escapeHTML $ queryText q) ++ runQuery dbs q ++ footer
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
runSuggest Search{queryParsed=Right Query{scope=[], names=[x], typeSig=Nothing}} = do
    root <- getDataDir
    db <- loadDataBase $ root </> "default.hoo"
    let res = take 8 $ completions db x
    return $ "[" ++ show x ++ "," ++ show res ++ "]"
runSuggest _ = return ""


-- is the package not something that might go wrong
safePackage :: String -> Bool
safePackage = all $ \x -> isAlphaNum x || x `elem` "-_"


-- return the databases you loaded, and those you can't
-- guarantees not to actually load the databases unless necessary
-- TODO: Should say which databases are ignored
loadDataBases :: CmdLine -> IO ([String], [DataBase])
loadDataBases Search{queryParsed=Right q} = do
    let pkgs = nub [x | PlusPackage x <- scope q, safePackage x]
        files = if null pkgs then ["default"] else pkgs
    root <- getDataDir
    files <- filterM doesFileExist $ map (\x -> root </> x <.> "hoo") files
    dbs <- unsafeInterleaveIO $ mapM loadDataBase files
    return ([], dbs)
loadDataBases _ = return ([], [])


-- TODO: Should escape the query text
runQuery :: [DataBase] -> CmdLine -> [String]
runQuery dbs Search{queryText = text, queryParsed = Left (pos,txt)} =
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


runQuery dbs q | not $ usefulQuery $ fromRight $ queryParsed q = welcome


runQuery dbs cq@Search{queryParsed = Right q} =
    ["<h1>Searching for " ++ qstr ++ "</h1>"] ++
    ["<p>" ++ showTagHTML (transform qurl sug) ++ "</p>" | Just sug <- [suggestQuery dbs q]] ++
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
        res = zip [0..] $ searchRange (rangeStartCount 0 (start2+count2+1)) dbs q
        (pre,res2) = splitAt start2 res
        (now,post) = splitAt count2 res2

        moreResults = "<tr><td></td><td><a href=\"" +& urlMore ++ "\" class='more'>Show more results</a></td></tr>"
        urlMore = "?hoogle=" +% queryText cq ++ "&start=" ++ show (start2+count2+1) ++ "#more"

        qstr = unwords $ ["<b>" +& n ++ "</b>" | n <- names q] ++
               ["::" | names q /= [] && isJust (typeSig q)] ++
               [showTagHTML (renderEntryText view $ renderTypeSig t) | Just t <- [typeSig q]]
        view = [ArgPosNum i i | i <- [0..10]]

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
renderRes i r =
        [tr $ modname ++ td "ans" (href urlEnt $ showTagHTMLWith url text)
        ,tr $ pkgname ++ td "doc" docs]
    where
        ent = fromLink $ resultEntry r
    
        (modu,text,_) = renderResult r
        modname = td "mod" $ maybe "" (href urlMod . showModule) modu
        pkgname = td "pkg" $ href urlPkg $ packageName $ fromLink $ entryPackage ent

        docs = ("<div id='d" ++ show i ++ "' class='shut'>" ++
                   "<a class='docs' onclick='return docs(" ++ show i ++ ")' href='" +& urlEnt ++ "'></a>") +?
                   (showTagHTML $ renderHaddock $ entryDocs ent) +?
               "</div>"

        urlPkg = entryPackageURL ent
        urlMod = entryModuleURL ent
        urlEnt = entryURL ent

        url (TagHyperlink _ x)
            | null urlEnt = Just $ "<span class='a'>" ++ showTagHTML x ++ "</span>"
            | otherwise = Just $ "</a><a href='" +& urlEnt ++ "'>" ++ showTagHTML x ++
                                 "</a><a class='dull' href='" +& urlEnt ++ "'>"
        url _ = Nothing

tr x = "<tr>" ++ x ++ "</tr>"
td c x = "<td" ++ (if null c then "" else " class='" ++ c ++ "'") ++ ">" ++ x ++ "</td>"
href url x = if null url then x else "<a class='dull' href='" +& url ++ "'>" ++ x ++ "</a>"
