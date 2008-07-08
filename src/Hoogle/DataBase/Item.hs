
-- TODO: Move Item and Haddock to Hoogle.Entry.*

module Hoogle.DataBase.Item(
    module Hoogle.DataBase.Item,
    module Hoogle.DataBase.Haddock
    ) where

import General.Code
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.DataBase.Haddock
import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import Data.Range
import Data.TagStr
import Data.Typeable


data Package = Package
    {packageName :: String
    ,packageVersion :: String
    ,haddockURL :: String
    ,hscolourURL :: String
    }

typename_Package = mkTyCon "Hoogle.DataBase.Item.Package"
instance Typeable Package where typeOf _ = mkTyConApp typename_Package []


data Module = Module
    {moduleName :: [String]
    ,modulePackage :: Link Package
    }

typename_Module = mkTyCon "Hoogle.DataBase.Item.Module"
instance Typeable Module where typeOf _ = mkTyConApp typename_Module []


-- invariant: entryName == head [i | Focus i <- entryText]
data Entry = Entry
    {entryModule :: Maybe (Link Module)
    ,entryName :: String
    ,entryText :: [EntryText]
    ,entryType :: EntryType
    ,entryDocs :: Haddock
    }

typename_Entry = mkTyCon "Hoogle.DataBase.Item.Entry"
instance Typeable Entry where typeOf _ = mkTyConApp typename_Entry []


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


entryScore :: Entry -> EntryScore
entryScore e = EntryScore
    (length m) (map toLower $ entryName e) (entryName e) m
    where m = maybe [] (moduleName . fromLink) $ entryModule e


renderEntryText :: [EntryView] -> [EntryText] -> TagStr
renderEntryText view = Tags . map f
    where
        f (Keyword x) = TagUnderline $ Str x
        f (Text x) = Str x
        f (ArgPos i s) = (if null res then id else TagColor (head res)) $ Str s
            where res = [k+1 | ArgPosNum k j <- view, j == i]
        f (ArgRes s) = TagColor 0 $ Str s
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


renderTextItem :: TextItem -> [EntryText]
renderTextItem x = case x of
    ItemClass i -> [Keyword "class", Text " "] ++ typeHead i
    ItemFunc name typ -> [Focus name, Text " :: "] ++ renderTypeSig typ
    ItemAlias a b -> [Keyword "type", Text " "] ++ typeHead a ++ [Text $ " = " ++ show b]
    ItemData d t -> [Keyword (show d), Text " "] ++ typeHead t
    where
        typeHead (TypeSig con sig) = [Text $ showConstraint con, Focus a, Text b]
            where (a,b) = break (== ' ') $ show sig


renderTypeSig :: TypeSig -> [EntryText]
renderTypeSig (TypeSig con sig) = Text (showConstraint con) :
    intersperse (Text " -> ") (zipWith ArgPos [0..] a ++ [ArgRes b])
    where (a,b) = initLast $ map show $ fromTFun sig


showModule = concat . intersperse "."

instance Show Package where
    show (Package a b c d) = unwords [a,b,c,d]

instance Show Module where
    show (Module a b) = unwords [showModule a, "{" ++ show b ++ "}"]

instance Show Entry where
    show (Entry a _ b _ _) = unwords [concatMap f b, m]
        where
            m = case a of
                    Nothing -> ""
                    Just y -> "{#" ++ show (linkKey y) ++ "}"

            f (Keyword x) = x
            f (Text x) = x
            f (Focus x) = x
            f (ArgPos _ x) = x
            f (ArgRes x) = x


instance BinaryDefer Package where
    put (Package a b c d) = put4 a b c d
    get = get4 Package

instance BinaryDefer Module where
    put (Module a b) = put2 a b
    get = get2 Module

instance BinaryDefer Entry where
    put (Entry a b c d e) = put5 a b c d e
    get = get5 Entry

instance BinaryDefer EntryText where
    put (Keyword a)  = putByte 0 >> put1 a
    put (Text a)     = putByte 1 >> put1 a
    put (Focus a)    = putByte 2 >> put1 a
    put (ArgPos a b) = putByte 3 >> put2 a b
    put (ArgRes a)   = putByte 4 >> put1 a

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
