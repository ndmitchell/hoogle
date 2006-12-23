
module General.HtmlTags(
    HtmlTag(..), parseHtmlTags,
    isOpenTagName, isCloseTagName
    ) where

import Data.Char
import Data.List


type HtmlAttr = (String,String)

data HtmlTag = OpenTag String [HtmlAttr]
             | CloseTag String
             | TextTag String
             deriving Show


parseHtmlTags :: String -> [HtmlTag]
parseHtmlTags [] = []

parseHtmlTags ('<':'/':xs) = CloseTag tag : parseHtmlTags trail
    where
        (tag,rest) = span isAlphaNum xs
        trail = drop 1 $ dropWhile (/= '>') rest

parseHtmlTags ('<':xs)
        | "/>" `isPrefixOf` rest2 = res : CloseTag tag : parseHtmlTags (drop 2 rest2)
        | ">" `isPrefixOf` rest2 = res : parseHtmlTags (drop 1 rest2)
        | otherwise = res : parseHtmlTags (drop 1 $ dropWhile (/= '>') rest2)
    where
        res = OpenTag tag attrs
        (tag,rest) = span isAlphaNum xs
        (attrs,rest2) = parseAttributes rest

parseHtmlTags (x:xs) = [TextTag $ parseString pre | not $ null pre] ++ parseHtmlTags post
    where (pre,post) = break (== '<') (x:xs)


parseAttributes :: String -> ([HtmlAttr], String)
parseAttributes (x:xs) | isSpace x = parseAttributes xs
                       | not $ isAlpha x = ([], x:xs)
                       | otherwise = ((parseString lhs, parseString rhs):attrs, over)
    where
        (attrs,over) = parseAttributes (dropWhile isSpace other)
    
        (lhs,rest) = span isAlphaNum (x:xs)
        rest2 = dropWhile isSpace rest
        (rhs,other) = if "=" `isPrefixOf` rest2 then parseValue (dropWhile isSpace $ tail rest2) else ("", rest2)
        

parseValue :: String -> (String, String)
parseValue ('\"':xs) = (a, drop 1 b)
    where (a,b) = break (== '\"') xs
parseValue x = span isAlphaNum x



escapes = [("gt",">")
          ,("lt","<")
          ,("amp","&")
          ,("quot","\"")
          ]


parseString :: String -> String
parseString ('&':xs) = case lookup a escapes of
                            Nothing -> '&' : parseString xs
                            Just x -> x ++ parseString (drop 1 b)
    where (a,b) = break (== ';') xs
parseString (x:xs) = x : parseString xs
parseString [] = []


isCloseTagName name (CloseTag n) = n == name
isCloseTagName _ _ = False


isOpenTagName name (OpenTag n _) = n == name
isOpenTagName _ _ = False
