{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.Item where

import General.Base
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.Type.Documentation
import Hoogle.Type.TagStr
import Hoogle.Type.TypeSig
import Data.Generics.Uniplate


type Input = ([Fact], [TextItem])


data TextItem = TextItem
    {itemLevel :: Int -- 0 = package, 1 = module, >2 = entry
    ,itemKey :: String -- how i should be searched for (name for most things, last module component for modules)
    ,itemName :: String -- what is the full text representation of me (key for most things, A.B.C for modules)
    ,itemType :: Maybe TypeSig
    ,itemDisp :: TagStr -- TagColor 0 for result type, TagColor 1.. for arg types, TagBold for name
    ,itemURL :: URL
    ,itemDocs :: String
    ,itemPriority :: Int -- priority, 0 is highest priority
    }
    deriving Show

data Fact
    = FactAlias TypeSig TypeSig
    | FactInstance TypeSig
    | FactDataKind String Int
    | FactClassKind String Int
    | FactCtorType String String -- Ctor, Data
      deriving Show


data Entry = Entry
    {entryParents :: [(Maybe (Link Entry), Maybe (Link Entry))] -- (package,module)
    ,entryName :: String
    ,entryText :: TagStr
    ,entryDocs :: Documentation
    ,entryURL :: URL
    ,entryPriority :: Int
    ,entryKey :: String -- used only for rebuilding combined databases
    ,entryType :: Maybe TypeSig -- used only for rebuilding combined databases
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

        g xs | ss /= [] = TagEmph (Str a) : g b
            where ss = filter (`isPrefixOf` map toLower xs) strs
                  (a,b) = splitAt (maximum $ map length ss) xs
        g (x:xs) = Str [x] : g xs
        g [] = []


-- TODO: EntryScore is over-prescriptive, and not overly useful
--       Have name and type scores to it themselves, using name only
--       to break ties when merging
-- the entry priority
-- the name of the entry, in lower case
-- the name of the entry
-- the module
data EntryScore = EntryScore Int String String String
                  deriving (Eq,Ord)


entryScore :: Entry -> EntryScore
entryScore e = EntryScore (entryPriority e) (map toLower $ entryName e) (entryName e) m
    where m = case entryParents e of (_,Just m):_ -> entryName $ fromLink m; _ -> ""

instance Show Entry where
    show = showTagText . entryText

instance BinaryDefer Entry where
    put (Entry a b c d e f g h) = put8 a b c d e f g h
    get = get8 Entry
