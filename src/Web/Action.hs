
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
    ,"  Query: <tt>" +? pre ++ "<span id='error'>" +? post2 ++ "</span></tt><br/>"
    ,"</p><p>"
    ,"  Error: " +? drop 1 (dropWhile (/= ':') $ show err) ++ "<br/>"
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


runQuery dbs CmdQuery{query = Right q} =
    ["<h1>Searching for " ++ qstr ++ "</h1>"] ++
    ["<p>" ++ showTagHTML sug ++ "</p>" | Just sug <- [suggestQuery dbs q]] ++
    if null res then
        ["<p>No results found</p>"]
    else
        ["<table>"] ++ concatMap renderRes res ++ ["</table>"]
    where
        res = searchRange (rangeStartCount 0 25) dbs q

        qstr = unwords $ ["<b>" ++ n ++ "</b>" | n <- names q] ++
               ["::" | names q /= [] && isJust (typeSig q)] ++
               [showTagHTML (renderEntryText view $ renderTypeSig t) | Just t <- [typeSig q]]
        view = [ArgPosNum i i | i <- [0..10]]



renderRes :: Result -> [String]
renderRes r =
        [tr $ td "mod" (maybe "" showModule modu) ++ td "" (showTagHTML text)
        ,tr $ td "pkg" pkg ++ td "doc" "todo"]
    where
        (modu,text,_) = renderResult r
        pkg = maybe "" (packageName . fromLink . modulePackage . fromLink) $ entryModule $ fromLink $ resultEntry r

        tr x = "<tr>" ++ x ++ "</tr>"
        td c x = "<td" ++ (if null c then "" else " class='" ++ c ++ "'") ++ ">" ++ x ++ "</td>"



a +? b = a ++ escapeHTML b


escapeHTML = concatMap f
    where
        f '\"' = "&quot;"
        f '<' = "&lt;"
        f '>' = "&gt;"
        f x = [x]


showTagHTML (Str x) = escapeHTML x
showTagHTML (Tags xs) = concatMap showTagHTML xs
showTagHTML (TagBold x) = "<b>" ++ showTagHTML x ++ "</b>"
showTagHTML (TagUnderline x) = "<i>" ++ showTagHTML x ++ "</i>"
showTagHTML (TagHyperlink url x) = "<a href=\"" +? url ++ "\">" ++ showTagHTML x ++ "</a>"
showTagHTML (TagColor i x) = "<span class='c" ++ show i ++ "'>" ++ showTagHTML x ++ "</span>"
