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

import General.CGI
import Web.Lambdabot
import Web.HTML

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Directory
import System.Info
import Control.Monad



---------------------------------------------------------------------
-- DEBUGGING SECTION

-- | Should the output be sent to the console and a file.
--   If true then both, the file is 'debugFile'.
--   Useful mainly for debugging.
--
debugForce = False

-- | Defaults to True always in Hugs, since no one will
--   run the service for real via Hugs, but I debug it that way
debugMode = debugForce || compilerName == "hugs"

-- | The file to output to if 'debugMode' is True
debugFile = "../../web/temp.htm"

-- | Clear the debugging file
debugBegin = when debugMode $ writeFile debugFile ""

-- | Write out a line, to console and optional to a debugging file
putLine :: String -> IO ()
putLine x = do putStrLn x
               when debugMode $ appendFile debugFile x




-- | The main function
main :: IO ()
main = do args <- cgiArgs
          putStr "Content-type: text/html\n\n"
          debugBegin
          appendFile "log.txt" (show args ++ "\n")
          let input = lookupDef "" "q" args
          
          let dat = WebData input
                            (if ("package","gtk") `elem` args then "gtk" else "")
                            (lookupDef "default" "logo" args)
          
          if null input then putLine $ htmlFront dat
           else do let p = hoogleParse input
                   case hoogleParseError p of
                        Just x -> putLine $ htmlError dat x
                        Nothing -> showResults dat p args


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


-- | Perform a search, dump the results using 'putLine'
showResults :: WebData -> Search -> [(String, String)] -> IO ()
showResults dat input args =
    do
        let useGtk = ("package","gtk") `elem` args
        res <- hoogleResults (if useGtk then "res/gtk.txt" else "res/hoogle.txt") input
        let lres = length res
            search = hoogleSearch input
            tSearch = showText search
            useres = take num $ drop start res

        let count =    
                "<table id='heading'><tr><td>" ++
                "Searched for " ++ showTags search ++
                "</td><td id='count'>" ++
                (if lres == 0 then "No results found" else f lres) ++
                "</td></tr></table>"
        
        let suggest = case hoogleSuggest True input of
                Nothing -> ""
                Just x -> "<p id='suggest'><span class='name'>Hoogle says:</span> " ++ showTags x ++ "</p>"

        lam <- Web.Lambdabot.query (lookupDef "" "q" args)
        let lambdabot = case lam of
                Nothing -> ""
                Just x -> "<p id='lambdabot'><span class='name'>" ++
                    "<a href='http://www.cse.unsw.edu.au/~dons/lambdabot.html'>Lambdabot</a> says:</span> "
                    ++ x ++ "</p>"

        let results = if null res then innerNoResult
                      else "<table id='results'>" ++ concatMap showResult useres ++ "</table>"
        
        let pageFlip = g lres
        
        let sher = if format == "sherlock" then sherlock useres else ""

        putLine $ htmlAnswers dat (count ++ suggest ++ lambdabot ++ results ++ pageFlip ++ sher)
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
showTags (Str x) = escapeHTML x
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
