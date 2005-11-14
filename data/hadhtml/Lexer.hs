
module Lexer(Lexeme(..), lexer) where

import TextUtil
import List
import Char

data Lexeme = Tag String [(String, String)]
            | ShutTag String
            | Text String
            deriving (Show)


entities = [("gt", '>'), ("lt", '<'), ("amp", '&')]

---------------------------------------------------------------------
-- MANIPULATORS
soupLower x = map f x
    where
        f (ShutTag x) = ShutTag (lcase x)
        f (Tag x xs) = Tag (lcase x) (map (\(a,b) -> (lcase a,b)) xs)
        f x = x
        
        lcase = map toLower

firstText (Text x:_) = x
firstText (x:xs) = firstText xs
firstText [] = ""

tagText (Text x) = x
tagText _ = ""

innerText x = concatMap tagText x

innerTextSpace x = oneSpace $ concatMap (\a -> ' ':tagText a) x

addAttr :: Lexeme -> (String, String) -> Lexeme
addAttr (Tag name attr) x = Tag name (x:attr)

getAttr :: Lexeme -> String -> Maybe String
getAttr (Tag _ attr) name = lookup name attr

---------------------------------------------------------------------
-- PARSER

lexer :: String -> [Lexeme]
lexer = {- map singleSpace . -} joinText . readTagSoup {- . map reSpace -}

reSpace x = if isSpace x then ' ' else x

joinText (Text a:Text b:xs) = joinText (Text (a ++ b):xs)
joinText (x:xs) = x : joinText xs
joinText [] = []

singleSpace (Text x) = Text (oneSpace x)
singleSpace x = x

oneSpace (' ':' ':xs) = oneSpace (' ':xs)
oneSpace (x:xs) = x : oneSpace xs
oneSpace [] = []

readTagSoup [] = []
readTagSoup ('<':'!':xs) = readDeclComment xs
readTagSoup ('<':'/':xs) = readShutTag xs
readTagSoup ('<':x:xs) | isAlpha x = readTag (x:xs)
readTagSoup ('&':xs) = readEntity xs
readTagSoup (x:xs) = Text [x] : readTagSoup xs


readDeclComment ('-':'-':xs) = readTagSoup (skipUntilAfter "-->" xs)
readDeclComment xs = readTagSoup (skipUntilAfter ">" xs)

readShutTag xs = ShutTag (trim a) : readTagSoup b
    where (a, b) = splitPairSafe ">" xs

readEntity xs | hasClose && valid = Text [value] : readTagSoup b
    where
        hasClose = ';' `elem` take 10 xs
        (a, b) = splitPairSafe ";" xs
        isNumber = head xs == '#'
        validNumber = all isDigit (tail a)
        validWord = all isAlpha a
        valid = (isNumber && validNumber) || (not isNumber && validWord)
        value = if isNumber then chr (read (tail a)) else maybe '?' id (lookup a entities)

readEntity xs = Text "&" : readTagSoup xs


isLegal x = isAlpha x || isDigit x || x == '_' || x == '-'


-- read until a space or a >, that is the tag
readTag x = readAttr (Tag a []) b
    where (a, b) = pairWhile isLegal x

--readAttr tag xs = error (take 100 xs)
readAttr tag ('>':xs) = tag : readTagSoup xs
readAttr tag (x:xs) | isSpace x = readAttr tag xs

readAttr tag [] = [tag]
readAttr tag (x:xs) | not (isLegal x) = error $ "Doh: " ++ show x ++ ": " ++ take 25 (x:xs)
readAttr tag xs = if head b == '='
                  then readString tag a (tail b)
                  else readAttr (addAttr tag (a, "")) b
    where (a, b) = pairWhile isLegal xs


readString tag name ('\"':xs) = readAttr (addAttr tag (name,a)) b
    where (a, b) = splitPairSafe "\"" xs

readString tag name xs = readAttr (addAttr tag (name,a)) b
    where (a, b) = pairWhile (\x -> x /= ' ' && x /= '>') xs


skipUntilAfter find str | find `isPrefixOf` str = drop (length find) str
skipUntilAfter find (x:xs) = skipUntilAfter find xs
skipUntilAfter find [] = []


splitPairSafe find str = case splitPair find str of
                              Just x -> x
                              Nothing -> (str, "")

pairWhile :: (a -> Bool) -> [a] -> ([a], [a])
pairWhile f xs = g [] xs
    where
        g done (x:xs) | f x = g (x:done) xs
        g done todo = (reverse done, todo)


