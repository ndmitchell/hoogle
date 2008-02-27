{-# OPTIONS_GHC -fglasgow-exts #-}

module Web.XML where

import General.CGI

-- the data

data XML = Tag String [Attribute] XML
         | PCData String
         | RawData String
         | XmlList [XML]

data Pair a b = a := b

data Attribute = Attribute String String


-- the generators

a <+> b = XmlList [a,b]

genTag :: (Maybe String, String) -> [Attribute] -> [XML] -> XML
genTag (a,b) c d = Tag b c (XmlList d)

toAttribute :: Pair String String -> Attribute
toAttribute (a := b) = Attribute a b

pcdata :: String -> XML
pcdata x = PCData x

genETag :: (Maybe String, String) -> [Attribute] -> XML
genETag a b = genTag a b []

rawXml x = RawData x

class ToXMLs a where
    toXMLs :: a -> XML

instance ToXMLs XML where
    toXMLs x = x

instance ToXMLs String where
    toXMLs x = PCData $ escapeHTML x

instance ToXMLs a => ToXMLs (Maybe a) where
    toXMLs Nothing = XmlList []
    toXMLs (Just x) = toXMLs x


-- the show

instance Show XML where
    show (Tag "hsx" [] inner) = show inner
    show (Tag name attr inner) = "<" ++ name ++ concatMap ((' ':) . show) attr ++ ">" ++
                                 show inner ++
                                 "</" ++ name ++ ">"
    show (PCData s) = s
    show (XmlList xs) = concatMap show xs
    show (RawData x) = x

instance Show Attribute where
    show (Attribute name value) = name ++ "=\"" ++ value ++ "\""

