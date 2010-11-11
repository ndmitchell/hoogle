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
        cols = [(a,b) | ArgPosNum a b <- view]
        strs = [map toLower x | FocusOn x <- view]

        f (TagColor i x) = maybe x (`TagColor` x) $ lookup i cols
        f (TagBold (Str xs)) = Tags $ g xs
        f (TagBold x) = x
        f x = x


        g xs | ss /= [] = TagBold (Str a) : g b
            where ss = filter (`isPrefixOf` map toLower xs) strs
                  (a,b) = splitAt (maximum $ map length ss) xs
        g (x:xs) = Str [x] : g xs
        g [] = []



{-
renderEntryText :: [EntryView] -> [EntryText] -> TagStr
renderEntryText view = Tags . map f
    where
        args = not $ null [() | ArgPosNum _ _ <- view]

        f (Keyword x) = TagUnderline $ Str x
        f (Text x) = Str x
        f (ArgPos i s) = (if null res then id else TagColor (head res)) $ Str s
            where res = [k+1 | ArgPosNum k j <- view, j == i]
        f (ArgRes s) = (if args then TagColor 0 else id) $ Str s
        f (Focus x) = TagHyperlink "" $ renderFocus [i | FocusOn i <- view] x


renderFocus :: [Range] -> String -> TagStr
renderFocus rs = Tags . f (mergeRanges rs) 0
    where
        str s = [Str s | s /= ""]

        f [] i s = str s
        f (r:rs) i s =
                str s1 ++ [TagBold $ Str s3] ++ f rs (rangeEnd r + 1) s4
            where
                (s1,s2) = splitAt (rangeStart r - i) s
                (s3,s4) = splitAt (rangeCount r) s2


renderTextItem :: TextItem -> [EntryText]
renderTextItem x = case x of
    ItemClass i -> [Keyword "class", Text " "] ++ typeHead i
    ItemFunc name typ -> operator name ++ [Text " :: "] ++ renderTypeSig typ
    ItemAlias a b -> [Keyword "type", Text " "] ++ typeHead a ++ [Text $ " = " ++ show b]
    ItemData d t -> [Keyword (show d), Text " "] ++ typeHead t
    where
        typeHead (TypeSig con sig) = [Text $ showConstraint con, Focus a, Text b]
            where (a,b) = break (== ' ') $ show sig

        operator xs@(x:_) | not $ isAlpha x || x `elem` "#_'" = [Text "(",Focus xs,Text ")"]
        operator xs = [Focus xs]


renderTypeSig :: TypeSig -> [EntryText]
renderTypeSig (TypeSig con sig) = Text (showConstraint con) :
    intersperse (Text " -> ") (zipWith ArgPos [0..] a ++ [ArgRes b])
    where (a,b) = initLast $ map showFun $ fromTFun sig
-}


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
