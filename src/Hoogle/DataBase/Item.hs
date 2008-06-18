
module Hoogle.DataBase.Item where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import Data.List
import General.All


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
    }


data EntryText = Keyword String
               | Text String
               | Focus String -- the bit text search starts from
               | ArgPos Int String
               | ArgRes String
                 deriving Show

data EntryView = FocusOn (Int,Int) -- characters (a,b) [a..b] should be focused
               | ArgPosNum Int Int -- argument a b, a is remapped to b
                 deriving Show


renderEntryText :: [EntryView] -> [EntryText] -> TagStr
renderEntryText view = Tags . map f
    where
        f (Keyword x) = TagBold $ Str x
        f (Text x) = Str x
        f (ArgPos i s) = Str s
        f (ArgRes s) = Str s
        
        -- TODO: very inefficient, should be improved
        --       should at least coallese adjacent Bold/Text's
        f (Focus x) = Tags [g $ Str [x] | (i,x) <- zip [0..] x
                           ,let g = if i `elem` bold then TagBold else id]
            where bold = concat [[i..j] | FocusOn (i,j) <- view]




showModule = concat . intersperse "."

instance Show Package where
    show (Package a b c d e) = unwords ["#" ++ show a,b,c,d,e]

instance Show Module where
    show (Module a b c) = unwords ["#" ++ show a, showModule b,
        "{" ++ show c ++ "}"]

instance Show Entry where
    show (Entry a b c d) = unwords ["#" ++ show a, concatMap f d, m]
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
    put (Entry a b c d) = put a >> put b >> put c >> put d
    get = get4 Entry

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
