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

module Doc.Main where

import Web.CGI
import Maybe
import Char
import List


main = do x <- cgiArgs
          let modu = lookup "module" x
              func = case lookup "func" x of
                         Nothing -> ""
                         Just a -> trimBrackets a
                         
          page <- case modu of
                      Just a -> calcPage a func
                      Nothing -> return failPage
          putStr $ "Location: " ++ page ++ "\n\n"
          


failPage = "nodocs.htm"


trimBrackets ('(':xs) = init xs
trimBrackets xs = xs


calcPage :: String -> String -> IO String
calcPage modu func = do x <- readFile "res/documentation.txt"
                        let xs = mapMaybe f $ lines x
                        return $ case lookup modu xs of
                            Just a -> urlPrefix ++ a ++ "/" ++ map g modu ++ ".html" ++ funcName
                            Nothing -> failPage
    where
        urlPrefix = "http://haskell.org/ghc/docs/latest/html/libraries/"
    
        f ys = case break (== '\t') ys of
                   (a, [] ) -> Nothing
                   (a, b) -> Just (a, dropWhile isSpace b)
                   
        funcName = if null func then "" else "#v%3A" ++ escape func
                   
        g '.' = '-'
        g x   = x

