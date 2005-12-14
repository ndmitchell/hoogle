{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{- |
    The Web interface, expects to be run as a CGI script.
    This does not require Haskell CGI etc, it just dumps HTML to the console
-}

module Web.Main where

import Hoogle.Hoogle
import Hoogle.TextUtil

import Web.CGI
import Web.Lambdabot

import Char
import System
import List
import Maybe
import Directory


-- | Should the output be sent to the console and a file.
--   If true then both, the file is 'debugFile'.
--   Useful mainly for debugging.
debugOut = False

fakeArgs :: IO [(String, String)]
fakeArgs = return $ [("q","map"), ("format","sherlock")]


-- | The main function
main :: IO ()
main = do args <- if debugOut then fakeArgs else cgiArgs
          putStr "Content-type: text/html\n\n"
          appendFile "log.txt" (show args ++ "\n")
          let input = lookupDef "" "q" args
          if null input then hoogleBlank
           else do let p = hoogleParse input
                   case hoogleParseError p of
                        Just x -> showError input x
                        Nothing -> showResults p args


lookupDef :: Eq key => val -> key -> [(key, val)] -> val
lookupDef def key list = case lookup key list of
                             Nothing -> def
                             Just x -> x

lookupDefInt :: Eq key => Int -> key -> [(key, String)] -> Int
lookupDefInt def key list = case lookup key list of
                              Nothing -> def
                              Just x -> case reads x of
                                           [(x,"")] -> x
                                           _ -> def


-- | Show the search box
hoogleBlank :: IO ()
hoogleBlank = do debugInit
                 outputFile "front"


-- | Replace all occurances of $ with the parameter
outputFileParam :: FilePath -> String -> IO ()
outputFileParam x param = do src <- readFile ("res/" ++ x ++ ".inc")
                             putLine (f src)
    where
        f ('$':xs) = param ++ f xs
        f (x:xs) = x : f xs
        f [] = []

outputFile :: FilePath -> IO ()
outputFile x = do src <- readFile ("res/" ++ x ++ ".inc")
                  putLine src


showError :: String -> String -> IO ()
showError input err =
    do
        debugInit
        outputFileParam "prefix" input
        outputFileParam "error" err
        outputFileParam "suffix" input
        


-- | Perform a search, dump the results using 'putLine'
showResults :: Search -> [(String, String)] -> IO ()
showResults input args =
    do
        res <- hoogleResults "res/hoogle.txt" input
        let lres = length res
            search = hoogleSearch input
            tSearch = showText search
            useres = take num $ drop start res

        debugInit
        outputFileParam "prefix" tSearch

        putLine $ 
            "<table id='heading'><tr><td>Searched for " ++ showTags search ++
            "</td><td id='count'>" ++
            (if lres == 0 then "No results found" else f lres) ++
            "</td></tr></table>"
        
        case hoogleSuggest True input of
            Nothing -> return ()
            Just x -> putLine $ "<p id='suggest'><span class='name'>Hoogle says:</span> " ++
                                showTags x ++ "</p>"

        lam <- Web.Lambdabot.query (lookupDef "" "q" args)
        case lam of
            Nothing -> return ()
            Just x -> putLine $ "<p id='lambdabot'><span class='name'>" ++
                "<a href='http://www.cse.unsw.edu.au/~dons/lambdabot.html'>Lambdabot</a> says:</span> "
                ++ x ++ "</p>"

        if null res then outputFileParam "noresults" tSearch
         else putLine $ "<table id='results'>" ++ concatMap showResult useres ++ "</table>"
        
        putLine $ g lres
        
        putLine $ if format == "sherlock" then sherlock useres else ""

        outputFileParam "suffix" tSearch
    where
        start = lookupDefInt 0 "start" args
        num   = lookupDefInt 25 "num"  args
        format = lookupDef "" "format" args
        nostart = filter ((/=) "start" . fst) args
        
        showPrev len pos = if start <= 0 then "" else
            "<a href='?" ++ asCgi (("start",show (max 0 (start-num))):nostart) ++ "'><img src='res/" ++ pos ++ "_left.png' /></a> "
        
        showNext len pos = if start+num >= len then "" else
            " <a href='?" ++ asCgi (("start",show (start+num)):nostart) ++ "'><img src='res/" ++ pos ++ "_right.png' /></a>"
        
    
        f len =
            showPrev len "top" ++
            "Results <b>" ++ show (start+1) ++ "</b> - <b>" ++ show (min (start+num) len) ++ "</b> of <b>" ++ show len ++ "</b>" ++
            showNext len "top"
        
        g len = if start == 0 && len <= num then "" else
            "<div id='select'>" ++
                showPrev len "bot" ++
                concat (zipWith h [1..10] [0,num..len]) ++
                showNext len "bot" ++
            "</div>"

        h num start2 = " <a " ++ (if start==start2 then "class='active' " else "") ++ "href='?" ++ asCgi (("start",show start2):nostart) ++ "'>" ++ show num ++ "</a> "
        
        

sherlock :: [Result] -> String
sherlock xs = "\n<!--\n<sherlock>\n" ++ concatMap f xs ++ "</sherlock>\n-->\n"
    where
        f res@(Result modu name typ _ _ _ _) =
            "<item>" ++ hoodoc res True ++
            "<abbr title='" ++ escapeHTML (showText typ) ++ "'>" ++ 
            showTags name ++ "</abbr> " ++
            "<span style='font-size:small;'>(" ++ showText modu ++ ")</span></a>" ++
            "</item>\n"


                 
showTags :: TagStr -> String
showTags (Str x) = x
showTags (Tag "b" x) = "<b>" ++ showTags x ++ "</b>"
showTags (Tag "u" x) = "<i>" ++ showTags x ++ "</i>"
showTags (Tag "a" x) = "<a href='" ++ url ++ "'>" ++ showTags x ++ "</a>"
    where
        url = if "http://" `isPrefixOf` txt then txt else "?q=" ++ escape txt
        txt = showText x
        
showTags (Tag [n] x) | n >= '1' && n <= '6' = 
    "<span class='c" ++ n : "'>" ++ showTags x ++ "</span>"
showTags (Tag n x) = showTags x
showTags (Tags xs) = concatMap showTags xs


showTagsLimit :: Int -> TagStr -> String
showTagsLimit n x = if length s > n then take (n-2) s ++ ".." else s
    where
        s = showText x


showResult :: Result -> String
showResult res@(Result modu name typ _ _ _ _) = 
    "<tr>" ++
        "<td class='mod'>" ++
            hoodoc res False ++ showTagsLimit 20 modu ++ "</a>" ++
            (if null (showTags modu) then "" else ".") ++
        "</td><td class='fun'>"
            ++ openA ++ showTags name ++ "</a>" ++
        "</td><td class='typ'>"
            ++ openA ++ ":: " ++ showTags typ ++ "</a>" ++
        "</td>" ++
    "</tr>\n"
        where
           openA = hoodoc res True


hoodoc :: Result -> Bool -> String
hoodoc res full = f $
        if not full
            then modu ++ "&amp;mode=module"
        else if resultMode res == "module"
            then modu ++ (if null modu then "" else ".") ++ showText (resultName res) ++ "&amp;mode=module"
        else showText (resultModule res) ++
             "&amp;name=" ++ escape (showText (resultName res)) ++
             "&amp;mode=" ++ resultMode res
    where
        modu = showText (resultModule res)
        f x = "<a href='hoodoc.cgi?module=" ++ x ++ "'>"


-- | The file to output to if 'debugOut' is True
debugFile = "temp.htm"


-- | Clear the debugging file
debugInit = if debugOut then writeFile debugFile "" else return ()

-- | Write out a line, to console and optional to a debugging file
putLine :: String -> IO ()
putLine x = do putStrLn x
               if debugOut then appendFile debugFile x else return ()


-- | Read the hit count, increment it, return the new value.
--   Hit count is stored in hits.txt
hitCount :: IO Integer
hitCount = do x <- readHitCount
              -- HUGS SCREWS THIS UP WITHOUT `seq`
              -- this should not be needed, but it is
              -- (we think)
              x `seq` writeHitCount (x+1)
              return (x+1)
    where
        hitFile = "hits.txt"
        
        readHitCount :: IO Integer
        readHitCount =
            do exists <- doesFileExist hitFile
               if exists
                   then do src <- readFile hitFile
                           return (parseHitCount src)
                   else return 0
        
        writeHitCount :: Integer -> IO ()
        writeHitCount x = writeFile hitFile (show x)
        
        parseHitCount = read . head . lines
              

-- | Take a piece of text and escape all the HTML special bits
escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f :: Char -> String
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f  x  = x:[]

