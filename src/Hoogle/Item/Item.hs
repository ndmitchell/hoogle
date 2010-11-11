{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Item.Item where

import General.Code
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.Item.Haddock
import Hoogle.TypeSig.All
import Data.TagStr
import Data.Typeable
import Data.Generics.Uniplate


data Package = Package
    {packageName :: String
    ,packageURL :: URL
    }
    deriving Typeable


data Module = Module
    {moduleName :: [String]
    ,modulePackage :: Link Package
    ,moduleURL :: URL
    }
    deriving Typeable


data Entry = Entry
    {entryModule :: Maybe (Link Module)
    ,entryPackage :: Link Package
    ,entryName :: String -- entirely pointless, should be eliminated by here!
    ,entryText :: TagStr
    ,entryDocs :: Haddock
    ,entryURL :: URL
    ,entryType :: Maybe TypeSig -- entirely pointless, should be eliminated by here!
    }
    deriving (Typeable)


data EntryView = FocusOn String -- characters in the range should be focused
               | ArgPosNum Int Int -- argument a b, a is remapped to b
                 deriving Show


renderEntryText :: [EntryView] -> TagStr -> TagStr
renderEntryText view = transform f
    where
        cols = [(b+1,a+1) | ArgPosNum a b <- view]
        strs = [map toLower x | FocusOn x <- view]

        f (TagColor i x) = maybe x (`TagColor` x) $ lookup i $ [(0,0)|cols/=[]] ++ cols
        f (TagBold (Str xs)) = TagBold $ Tags $ g xs
        f x = x

        g xs | ss /= [] = TagUnderline (Str a) : g b
            where ss = filter (`isPrefixOf` map toLower xs) strs
                  (a,b) = splitAt (maximum $ map length ss) xs
        g (x:xs) = Str [x] : g xs
        g [] = []


-- TODO: EntryScore is over-prescriptive, and not overly useful
--       Have name and type scores to it themselves, using name only
--       to break ties when merging
-- the number of elements in the module name
-- the name of the entry, in lower case
-- the name of the entry
-- the module
data EntryScore = EntryScore Int String String [String]
                  deriving (Eq,Ord)


entryScore :: Entry -> EntryScore
entryScore e = EntryScore
    (length m)
    (map toLower $ entryName e) (entryName e) m
    where m = maybe [] (moduleName . fromLink) $ entryModule e



showModule = concat . intersperse "."

instance Show Package where
    show (Package a b) = unwords $ filter (/= "") [a,b]

instance Show Module where
    show = showModule . moduleName

instance Show Entry where
    show e = unwords [showTagText $ entryText e, m]
        where
            m = case entryModule e of
                    Nothing -> ""
                    Just y -> "{#" ++ show (linkKey y) ++ "}"


instance BinaryDefer Package where
    put (Package a b) = put2 a b
    get = get2 Package

instance BinaryDefer Module where
    put (Module a b c) = put3 a b c
    get = get3 Module

instance BinaryDefer Entry where
    put (Entry a b c d e f g) = put7 a b c d e f g
    get = get7 Entry
