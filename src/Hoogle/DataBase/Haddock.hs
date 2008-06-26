
module Hoogle.DataBase.Haddock(
    Haddock, newHaddock, renderHaddock
    ) where

import General.Code
import Data.TagStr
import Data.Binary.Defer
import Data.Binary.Defer.Vector


newtype Haddock = Haddock (Vector Char)


instance BinaryDefer Haddock where
    put (Haddock x) = put x
    get = get1 Haddock


newHaddock = Haddock . fromList


renderHaddock :: Haddock -> TagStr
renderHaddock (Haddock xs) = Str $ toList xs



---------------------------------------------------------------------
-- PARSER

type Tags = [Tag]
data Tag = Char Char | Tag String Tags
           deriving Show

parseHaddock :: String -> Tags
parseHaddock = fst . readHaddock ">"


readHaddock :: String -> String -> (Tags, String)
readHaddock name = f
    where
        f ('&':'a':'m':'p':';':xs) = g xs $ Char '&'
        f ('&':'g':'t':';':xs) = g xs $ Char '>'
        f ('&':'l':'t':';':xs) = g xs $ Char '<'
        f ('<':'/':xs) | a == name = ([], drop 1 b)
            where (a,b) = break (== '>') xs
        f ('<':xs) | not $ "/" `isPrefixOf` xs = g d $ Tag a c
            where (a,b) = break (== '>') xs
                  (c,d) = readHaddock a $ drop 1 b
        f (x:xs) = g xs $ Char x
        f [] = ([],[])

        g rest add = (add:a,b)
            where (a,b) = f rest
