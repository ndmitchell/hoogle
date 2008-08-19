
module Hoogle.Item.Haddock(
    Haddock, newHaddock, renderHaddock
    ) where

import General.Code
import Data.TagStr
import Data.Binary.Defer
import Data.ByteString.Char8(ByteString,pack,unpack)


-- TODO: Should be a bytestring, then use hPut from ByteString
--       to write out faster
newtype Haddock = Haddock ByteString


instance BinaryDefer Haddock where
    put (Haddock x) = put x
    get = get1 Haddock


newHaddock = Haddock . pack


renderHaddock :: Haddock -> TagStr
renderHaddock (Haddock xs) = Tags $ f False $ parseHaddock $ unpack xs
    where
        nl = Char '\n'

        -- boolean, are you in a pre block
        f False (Char '\n':Char '\n':xs) = Str "\n\n" : f False (dropWhile (== nl) xs)
        f False (Char '\n':xs) = Str " " : f False xs

        f True (Char '\n':xs) = Str "\n" : Str "> " : f True xs

        -- TODO: tt is ignored, add a TagMonospage?
        f pre (Tag "tt" x:xs) = f pre (x++xs)
        f pre (Tag [t,'l'] x:xs) | t `elem` "ou" = tail $ f pre (filter (/= nl) x ++ xs)
        f pre (Tag "pre" x:xs) = init (init $ tail $ f True x) ++ f pre xs
        f pre (Tag "li" x:xs) = Str "\n" : Str "* " : f pre x ++ f pre xs
        f pre (Tag "a" x:xs) = TagHyperlink "" (Tags $ f pre x) : f pre xs
        f pre (Tag "i" x:xs) = TagUnderline (Tags $ f pre x) : f pre xs
        f pre (Tag "b" x:xs) = TagBold (Tags $ f pre x) : f pre xs

        f pre (Tag n x:xs) = Str (show (Tag n x)) : f pre xs
        f pre (Char x:xs) = Str [x] : f pre xs
        f pre [] = []



---------------------------------------------------------------------
-- PARSER

type Tags = [Tag]
data Tag = Char Char | Tag String Tags
           deriving (Eq,Show)

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
