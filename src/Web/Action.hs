
module Web.Action(actionWeb) where

import CmdLine.All
import Hoogle.All
import Hoogle.Query.All
import Hoogle.Item.All
import Hoogle.Search.All
import General.Code
import System.IO.Unsafe(unsafeInterleaveIO)
import Web.Page
import Text.ParserCombinators.Parsec
import Data.TagStr
import Data.Range
import Data.Binary.Defer.Index


actionWeb :: CmdQuery -> IO ()
actionWeb q = do
    (skipped,dbs) <- loadDataBases q
    let res = unlines $ header (escapeHTML $ queryText q) ++ runQuery dbs q ++ footer
    when (Debug `elem` queryFlags q) $
        writeFile "temp.htm" res
    putStrLn res


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
    ,"  For information on what queries should look like, see the user manual."
    ,"</p>"
    ]
    where
        (pre,post) = splitAt (sourceColumn (errorPos err) - 1) text
        post2 = if null post then concat (replicate 3 "&nbsp;") else post


runQuery dbs q | not $ usefulQuery $ fromRight $ query q =
    ["<h1><b>Welcome to Hoogle</b></h1>"
    ,"<p>"
    ,"  Hoogle is a Haskell API search engine, have fun!"
    ,"</p>"
    ]


runQuery dbs cq@CmdQuery{query = Right q, queryFlags = flags} =
    ["<h1>Searching for " ++ qstr ++ "</h1>"] ++
    ["<p>" ++ showTagHTML sug ++ "</p>" | Just sug <- [suggestQuery dbs q]] ++
    if null res then
        ["<p>No results found</p>"]
    else -- error $ show (length res, length pre, length now, length post) -
        ["<table>"] ++
        concatMap renderRes pre ++
        insertMore (concatMap renderRes now) ++
        [moreResults | not $ null post] ++
        ["</table>"]
    where
        start = headDef 0 [i-1 | Start i <- flags]
        count = headDef 20 [n | Count n <- flags]
        res = searchRange (rangeStartCount 0 (start+count+1)) dbs q
        (pre,res2) = splitAt start res
        (now,post) = splitAt count res2

        moreResults = "<tr><td></td><td><a href='" ++ urlMore ++ "' class='more'>Show more results</a></td></tr>"
        urlMore = "?q=" +% queryText cq ++ "&start=" ++ show (start+count+1) ++ "#more"

        qstr = unwords $ ["<b>" +& n ++ "</b>" | n <- names q] ++
               ["::" | names q /= [] && isJust (typeSig q)] ++
               [showTagHTML (renderEntryText view $ renderTypeSig t) | Just t <- [typeSig q]]
        view = [ArgPosNum i i | i <- [0..10]]


-- insert <a name=more> where you can
insertMore :: [String] -> [String]
insertMore [] = []
insertMore (x:xs) = f x : xs
    where
        f ('>':xs) | not $ "<td" `isPrefixOf` xs = "><a name='more'></a>" ++ xs
        f (x:xs) = x : f xs
        f [] = []


renderRes :: Result -> [String]
renderRes r =
        [tr $ td "mod" modname ++ td "" (href urlItem $ showTagHTMLWith url text)
        ,tr $ td "pkg" pkgname ++ td "doc" doc]
    where
        pkg = liftM (fromLink . modulePackage . fromLink) $ entryModule $ fromLink $ resultEntry r
    
        (modu,text,_) = renderResult r
        modname = maybe "" (href urlModule . showModule) modu
        pkgname = maybe "" (href urlPkg . packageName) pkg
        doc = takeWhile (/= '\n') $ showTagHTML $ renderHaddock $ entryDocs $ fromLink $ resultEntry r

        urlPkg = "http://hackage.haskell.org/packages/archive/" +? maybe "" packageName pkg +? "/latest/doc/html/"
        urlModule = urlPkg +? concat (intersperse "-" $ fromMaybe [] modu) +? ".html"
        urlItem = urlModule +? "#v:" +? escapeHTML (entryName $ fromLink $ resultEntry r)

        url (TagHyperlink _ x) = Just $ "</a><a href='" +& urlItem ++ "'>" ++ showTagHTML x ++
                                        "</a><a class='dull' href='" +& urlItem ++ "'>"
        url _ = Nothing

tr x = "<tr>" ++ x ++ "</tr>"
td c x = "<td" ++ (if null c then "" else " class='" ++ c ++ "'") ++ ">" ++ x ++ "</td>"
href url x = if null url then x else "<a class='dull' href='" ++ url ++ "'>" ++ x ++ "</a>"


-- | Only append strings if neither one is empty
(+?) :: String -> String -> String
a +? b = if null a || null b then [] else a ++ b

-- | Escape the second argument before appending
(+&) :: String -> String -> String
a +& b = a ++ escapeHTML b

(+%) = (+&) -- CGI query string escaping


escapeHTML = concatMap f
    where
        f '\"' = "&quot;"
        f '<' = "&lt;"
        f '>' = "&gt;"
        f x = [x]


showTagHTML = showTagHTMLWith (const Nothing)


showTagHTMLWith :: (TagStr -> Maybe String) -> TagStr -> String
showTagHTMLWith f x = g x
    where
        g x | isJust (f x) = fromJust $ f x
        g (Str x) = escapeHTML x
        g (Tags xs) = concatMap g xs
        g (TagBold x) = "<b>" ++ showTagHTML x ++ "</b>"
        g (TagUnderline x) = "<i>" ++ showTagHTML x ++ "</i>"
        g (TagHyperlink url x) = "<a href=\"" +& url ++ "\">" ++ showTagHTML x ++ "</a>"
        g (TagColor i x) = "<span class='c" ++ show i ++ "'>" ++ showTagHTML x ++ "</span>"
