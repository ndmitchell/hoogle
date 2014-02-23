{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hoogle.Type.Docs(
    Docs(..), readDocsHTML, renderDocs
    ) where

import General.Base
import Hoogle.Type.TagStr
import Hoogle.Store.All
import Data.ByteString.Char8(ByteString,pack,unpack)
import Data.Binary


newtype Docs = Docs {fromDocs :: ByteString}
    deriving (Eq,Ord,Binary,Monoid)


instance Store Docs where
    put (Docs x) = put1 x
    get = get1 Docs

readDocsHTML :: String -> Docs
readDocsHTML = Docs . pack


renderDocs :: Docs -> TagStr
renderDocs (Docs xs) = tags $ f False $ parseHTML $ unpack xs
    where
        nl = Char '\n'

        -- boolean, are you in a pre block
        f False (Char '\n':Char '\n':xs) = Str "\n\n" : f False (dropWhile (== nl) xs)
        f False (Char '\n':xs) = Str " " : f False xs

        f True (Char '\n':xs) = Str "\n" : Str "> " : f True xs

        -- TODO: tt is ignored, add a TagMonospage?
        f pre (Tag "tt" x:xs) = f pre (x++xs)
        f pre (Tag [t,'l'] x:xs) | t `elem` "ou" = tail $ f pre (filter (/= nl) x ++ xs)
        f pre (Tag "pre" x:xs) = let ys = init $ tail $ f True x
                                 in if null ys then ys else init ys ++ f pre xs
        f pre (Tag "li" x:xs) = Str "\n" : Str "* " : f pre x ++ f pre xs
        f pre (Tag "a" x:xs) = TagLink "" (tags $ f pre x) : f pre xs
        f pre (Tag "i" x:xs) = TagEmph (tags $ f pre x) : f pre xs
        f pre (Tag "em" x:xs) = TagEmph (tags $ f pre x) : f pre xs
        f pre (Tag "b" x:xs) = TagBold (tags $ f pre x) : f pre xs

        f pre (Tag n x:xs) = Str (show (Tag n x)) : f pre xs
        f pre (Char x:xs) = Str [x] : f pre xs
        f pre [] = []



---------------------------------------------------------------------
-- PARSER

type Tags = [Tag]
data Tag = Char Char | Tag String Tags
           deriving (Eq,Show)

parseHTML :: String -> Tags
parseHTML = fst . readHTML ">"


readHTML :: String -> String -> (Tags, String)
readHTML name = f
    where
        f ('&':'a':'m':'p':';':xs) = g xs $ Char '&'
        f ('&':'g':'t':';':xs) = g xs $ Char '>'
        f ('&':'l':'t':';':xs) = g xs $ Char '<'
        f ('<':'/':xs) | a == name = ([], drop 1 b)
            where (a,b) = break (== '>') xs
        f ('<':xs) | not $ "/" `isPrefixOf` xs = g d $ Tag a c
            where (a,b) = break (== '>') xs
                  (c,d) = readHTML a $ drop 1 b
        f (x:xs) = g xs $ Char x
        f [] = ([],[])

        g rest add = (add:a,b)
            where (a,b) = f rest
