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

import Hoogle.Match
import Hoogle.TextUtil
import Hoogle.TypeSig

import Web.CGI

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
main = do x <- if debugOut then fakeArgs else cgiArgs
          putStr "Content-type: text/html\n\n"
          appendFile "log.txt" (show x ++ "\n")
          let args = lookupDef "" "q" x
          if null args then hoogleBlank else hoogle args x


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


-- | Perform a search, dump the results using 'putLine'
hoogle :: String -> [(String, String)] -> IO ()
hoogle args other =
    do
    
        debugInit
        outputFileParam "prefix" args
        raw <- matchOrdered "res/hoogle.txt" args
        let (err, res) = case raw of
                              Left x -> ("error: " ++ err, [])
                              Right x -> ("", x)
            lres = length res
            search = formatSearchString args
            useres = take num $ drop start res

        putLine $ 
            "<table id='heading'><tr><td>Searched for " ++ showTags search ++
            "</td><td id='count'>" ++
            (if lres == 0 then "No results found" else f lres) ++
            "</td></tr></table>"

        if not (null err) then outputFileParam "error" args
            else if null res then outputFileParam "noresults" args
            else putLine $ "<table id='results'>" ++ concatMap showResult useres ++ "</table>"
        
        putLine $ g lres
        
        putLine $ if format == "sherlock" then sherlock useres else ""

        outputFileParam "suffix" args
    where
        start = lookupDefInt 0 "start" other
        num   = lookupDefInt 25 "num"  other
        format = lookupDef "" "format" other
        nostart = filter ((/=) "start" . fst) other
        
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
        f (Result modu name typ _ _ _) =
            "<item>" ++ hoodoc modu (Just name) ++
            "<abbr title='" ++ escapeHTML (showText typ) ++ "'>" ++ 
            showTags name ++ "</abbr> " ++
            "<span style='font-size:small;'>(" ++ showText modu ++ ")</span></a>" ++
            "</item>\n"


                 
showTags :: TagString -> String
showTags (Str x) = x
showTags (Tag 0 x) = "<b>" ++ showTags x ++ "</b>"
showTags (Tag n x) = "<span class='c" ++ show n ++ "'>" ++ showTags x ++ "</span>"
showTags (Tags xs) = concatMap showTags xs


showTagsLimit :: Int -> TagString -> String
showTagsLimit n x = if left < 0 then res ++ ".." else res
    where
        (left, res) = f n x
        
        f n (Str x) = if lx > n then (-1, take n x) else (n - lx, x)
            where lx = length x
        
        f n (Tag 0 x) = (left, "<b>" ++ res ++ "</b>")
            where (left, res) = f n x
        f n (Tag c x) = (left, "<span class='c" ++ show c ++ "'>" ++ res ++ "</span>")
        
        f n (Tags []) = (n, "")
        f n (Tags (x:xs)) = if left == -1 then (left, res) else (left2, res ++ res2)
            where
                (left, res) = f n x
                (left2, res2) = f left (Tags xs)
        


showResult :: Result -> String
showResult (Result modu name typ _ _ _) = 
    "<tr>" ++
        "<td class='mod'>" ++
            hoodoc modu Nothing ++ showTagsLimit 20 modu ++ "</a>." ++
        "</td><td class='fun'>"
            ++ openA ++ showTags name ++ "</a>" ++
        "</td><td class='typ'>"
            ++ openA ++ ":: " ++ showTags typ ++ "</a>" ++
        "</td>" ++
    "</tr>\n"
        where
           openA = hoodoc modu (Just name)


hoodoc :: TagString -> Maybe TagString -> String
hoodoc modu func = case func of
                        Nothing -> f $ showText modu
                        Just x -> f $ showText modu ++ "&amp;func=" ++ escape (showText x)
    where f x = "<a href='hoodoc.cgi?module=" ++ x ++ "'>"


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

