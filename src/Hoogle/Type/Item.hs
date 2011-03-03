{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Hoogle.Type.Item where

import General.Base
import General.Util
import Hoogle.Store.All
import Hoogle.Type.Docs
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


-- Invariant: locations will not be empty
data Entry = Entry
    {entryLocations :: [(URL, [Once Entry])]
    ,entryName :: String
    ,entryText :: TagStr
    ,entryDocs :: Docs
    ,entryPriority :: Int
    ,entryKey :: String -- used only for rebuilding combined databases
    ,entryType :: Maybe TypeSig -- used only for rebuilding combined databases
    }
    deriving Typeable


-- | Figure out what makes this entry different from others
entryUnique Entry{..} = (entryName, entryText, entryDocs, entryKey, entryType)


-- | Join two entries that are equal under entryUnique
entryJoin e1 e2 = e1
    {entryPriority = min (entryPriority e1) (entryPriority e2)
    ,entryLocations = nubOn (map (entryName . fromOnce) . snd) $ concatMap entryLocations $
        if entryScore e1 < entryScore e2 then [e1,e2] else [e2,e1]}


entryURL e = head $ map fst (entryLocations e) ++ [""]


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
data EntryScore = EntryScore Int String String
                  deriving (Eq,Ord)


entryScore :: Entry -> EntryScore
entryScore e = EntryScore (entryPriority e) (map toLower $ entryName e) (entryName e)

instance Show Entry where
    show = showTagText . entryText

instance Store Entry where
    put (Entry a b c d e f g) = put7 a b c d e f g
    get = get7 Entry
