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
          let modu = fromJust $ lookup "module" x
              mode = fromJust $ lookup "mode" x
              name = case lookup "name" x of
                        Nothing -> ""
                        Just ('(':xs) -> init xs
                        Just x -> x
                        
          page <- hoodoc mode modu name
          putStr $ "Location: " ++ page ++ "\n\n"


hoodoc :: String -> String -> String -> IO String
hoodoc "module" modu name = calcPage modu

hoodoc "keyword" modu name = return "http://www.haskell.org/hawiki/Keywords"

hoodoc "func"  modu name = do x <- calcPage modu
                              return $ x ++ "#v%3A" ++ escape name
                               
hoodoc _  modu name = do x <- calcPage modu
                         return $ x ++ "#t%3A" ++ escape name



failPage = "nodocs.htm"


calcPage :: String -> IO String
calcPage modu = do x <- readFile "res/documentation.txt"
                   let xs = mapMaybe f $ lines x
                   return $ case lookup modu xs of
                       Just a -> urlPrefix ++ a ++ "/" ++ map g modu ++ ".html"
                       Nothing -> failPage
    where
        urlPrefix = "http://haskell.org/ghc/docs/latest/html/libraries/"
    
        f ys = case break (== '\t') ys of
                   (a, [] ) -> Nothing
                   (a, b) -> Just (a, dropWhile isSpace b)
                   
        g '.' = '-'
        g x   = x

