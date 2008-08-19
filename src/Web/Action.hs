
module Web.Action(actionWeb) where

import CmdLine.All
import Hoogle.All
import Hoogle.Query.All
import Hoogle.Item.All
import Hoogle.Search.All
import Numeric
import General.Code
import System.IO.Unsafe(unsafeInterleaveIO)
import Web.Page
import Web.Text
import Text.ParserCombinators.Parsec
import Data.TagStr
import Data.Range
import Data.Binary.Defer.Index
import Data.Generics.Uniplate

import Data.Time.Clock
import Data.Time.Calendar
import General.CGI(cgiArgs)


actionWeb :: CmdQuery -> IO ()
actionWeb q = do
    putStr "Content-type: text/html\n\n"
    logMessage q
    (skipped,dbs) <- loadDataBases q
    let res = unlines $ header (escapeHTML $ queryText q) ++ runQuery dbs q ++ footer
    putStrLn res
    when (Debug `elem` queryFlags q) $
        writeFile "temp.htm" res


logMessage :: CmdQuery -> IO ()
logMessage q = do
    time <- getCurrentTime
    cgi <- liftM (fromMaybe []) $ cgiArgs
    appendFile "res/log.txt" $ (++ "\n") $ unwords $
        [showGregorian (utctDay time)
        ,show (queryText q)] ++
        ["?" ++ a ++ "=" ++ c ++ b ++ c | (a,b) <- cgi, let c = ['\"' | any isSpace b]]


-- is the package not something that might go wrong
safePackage :: String -> Bool
safePackage = all $ \x -> isAlphaNum x || x `elem` "-_"


-- return the databases you loaded, and those you can't
-- guarantees not to actually load the databases unless necessary
-- TODO: Should say which databases are ignored
loadDataBases :: CmdQuery -> IO ([String], [DataBase])
loadDataBases CmdQuery{query=Right q} = do
    let pkgs = nub [x | PlusPackage x <- scope q, safePackage x]
        files = if null pkgs then ["default"] else pkgs
    files <- filterM doesFileExist $ map (\x -> "res" </> x <.> "hoo") files
    dbs <- unsafeInterleaveIO $ mapM loadDataBase files
    return ([], dbs)
loadDataBases _ = return ([], [])


-- TODO: Should escape the query text
runQuery :: [DataBase] -> CmdQuery -> [String]
runQuery dbs CmdQuery{queryText = text, query = Left err} =
    ["<h1><b>Parse error in user query</b></h1>"
    ,"<p>"
    ,"  Query: <tt>" +& pre ++ "<span id='error'>" +& post2 ++ "</span></tt><br/>"
    ,"</p><p>"
    ,"  Error: " +& drop 1 (dropWhile (/= ':') $ show err) ++ "<br/>"
    ,"</p><p>"
    ,"  For information on what queries should look like, see the"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>user manual</a>."
    ,"</p>"
    ]
    where
        (pre,post) = splitAt (sourceColumn (errorPos err) - 1) text
        post2 = if null post then concat (replicate 3 "&nbsp;") else post


runQuery dbs q | not $ usefulQuery $ fromRight $ query q = welcome


runQuery dbs cq@CmdQuery{query = Right q, queryFlags = flags} =
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
        start = headDef 0 [i-1 | Start i <- flags]
        count = headDef 20 [n | Count n <- flags]
        res = zip [0..] $ searchRange (rangeStartCount 0 (start+count+1)) dbs q
        (pre,res2) = splitAt start res
        (now,post) = splitAt count res2

        moreResults = "<tr><td></td><td><a href=\"" ++ urlMore ++ "\" class='more'>Show more results</a></td></tr>"
        urlMore = "?q=" +% queryText cq ++ "&start=" ++ show (start+count+1) ++ "#more"

        qstr = unwords $ ["<b>" +& n ++ "</b>" | n <- names q] ++
               ["::" | names q /= [] && isJust (typeSig q)] ++
               [showTagHTML (renderEntryText view $ renderTypeSig t) | Just t <- [typeSig q]]
        view = [ArgPosNum i i | i <- [0..10]]

        qurl (TagHyperlink url x) | "query:" `isPrefixOf` url = TagHyperlink ("?q=" +% drop 6 url) x
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
        [tr $ modname ++ td "" (href urlEnt $ showTagHTMLWith url text)
        ,tr $ pkgname ++ td "doc" docs]
    where
        ent = fromLink $ resultEntry r
        pkg = liftM fromLink $ entryPackage ent
    
        (modu,text,_) = renderResult r
        modname = td "mod" $ maybe "" (href urlMod . showModule) modu
        pkgname = td "pkg" $ maybe "" (href urlPkg . packageName) pkg

        docs = if length docLong == 0 then "" else
               if docShort == docLong then docShort else
               "<div id='s" ++ show i ++ "'>" ++
                    "<a class='more' href='" ++ urlEnt ++ "' onclick='return doc_more(" ++ show i ++ ")'> </a>" ++
                    docShort ++
               "</div>" ++
               "<div style='display:none' id='l" ++ show i ++ "'>" ++
                    "<a class='less' onclick='return doc_less(" ++ show i ++ ")'> </a>" ++
                    docLong ++
               "</div>"

        doc = renderHaddock $ entryDocs $ ent
        docShort = showTagHTML $ trimTags 100 $ transform (onStr $ map (rep '\n' ' ')) doc
        docLong = showTagHTML doc

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
href url x = if null url then x else "<a class='dull' href='" ++ url ++ "'>" ++ x ++ "</a>"
