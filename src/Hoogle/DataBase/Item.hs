
module Hoogle.DataBase.Item(
    module Hoogle.DataBase.Item,
    module Hoogle.DataBase.Haddock
    ) where

import General.Code
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.DataBase.Haddock
import Data.Range
import Data.TagStr


data Package = Package
    {packageId :: Id
    ,packageName :: String
    ,packageVersion :: String
    ,haddockURL :: String
    ,hscolourURL :: String
    }


data Module = Module
    {moduleId :: Id
    ,moduleName :: [String]
    ,modulePackage :: Lookup Package
    }


-- invariant: entryName == head [i | Focus i <- entryText]
data Entry = Entry
    {entryId :: Id
    ,entryModule :: Maybe (Lookup Module)
    ,entryName :: String
    ,entryText :: [EntryText]
    ,entryType :: EntryType
    ,entryDocs :: Haddock
    }


data EntryText = Keyword String
               | Text String
               | Focus String -- the bit text search starts from
               | ArgPos Int String
               | ArgRes String
                 deriving Show

data EntryView = FocusOn Range -- characters in the range should be focused
               | ArgPosNum Int Int -- argument a b, a is remapped to b
                 deriving Show

data EntryType = EntryModule
               | EntryKeyword
               | EntryOther
                 deriving (Eq,Enum,Show)

-- the number of elements in the module name
-- the name of the entry, in lower case
-- the name of the entry
-- the module
data EntryScore = EntryScore Int String String [String]
                  deriving (Eq,Ord)


entryScore :: Index Module -> Entry -> EntryScore
entryScore mods e = entryScoreModule (liftM (`lookupIndex` mods) (entryModule e)) e


entryScoreModule :: Maybe Module -> Entry -> EntryScore
entryScoreModule mod e = EntryScore
    (length m) (map toLower $ entryName e) (entryName e) m
    where m = maybe [] moduleName mod


renderEntryText :: [EntryView] -> [EntryText] -> TagStr
renderEntryText view = Tags . map f
    where
        f (Keyword x) = TagUnderline $ Str x
        f (Text x) = Str x
        f (ArgPos i s) = Str s
        f (ArgRes s) = Str s
        f (Focus x) = renderFocus [i | FocusOn i <- view] x


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


showModule = concat . intersperse "."

instance Show Package where
    show (Package a b c d e) = unwords ["#" ++ show a,b,c,d,e]

instance Show Module where
    show (Module a b c) = unwords ["#" ++ show a, showModule b,
        "{" ++ show c ++ "}"]

instance Show Entry where
    show (Entry a b c d _ _) = unwords ["#" ++ show a, concatMap f d, m]
        where
            m = case b of
                    Nothing -> ""
                    Just y -> "{" ++ show y ++ "}"

            f (Keyword x) = x
            f (Text x) = x
            f (Focus x) = x
            f (ArgPos _ x) = x
            f (ArgRes x) = x


instance BinaryDefer Package where
    put (Package a b c d e) = put a >> put b >> put c >> put d >> put e
    get = get5 Package

instance BinaryDefer Module where
    put (Module a b c) = put a >> put b >> put c
    get = get3 Module

instance BinaryDefer Entry where
    put (Entry a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
    get = get6 Entry

instance BinaryDefer EntryText where
    put (Keyword a)  = putByte 0 >> put a
    put (Text a)     = putByte 1 >> put a
    put (Focus a)    = putByte 2 >> put a
    put (ArgPos a b) = putByte 3 >> put a >> put b
    put (ArgRes a)   = putByte 4 >> put a

    get = do i <- getByte
             case i of
                0 -> get1 Keyword
                1 -> get1 Text
                2 -> get1 Focus
                3 -> get2 ArgPos
                4 -> get1 ArgRes

instance BinaryDefer EntryType where
    put = put . fromEnum
    get = liftM toEnum get
