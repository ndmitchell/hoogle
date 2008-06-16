
module Hoogle.Item.All where

import Data.Binary.Defer.Index
import Data.List


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


data Entry = Entry
    {entryId :: Id
    ,entryModule :: Maybe (Lookup Module)
    ,entryText :: [EntryText]
    }


data EntryText = Keyword String
               | Text String
               | Focus String -- the bit text search starts from
               | ArgPos Int String
               | Result String
                 deriving Show


showModule = concat . intersperse "."

instance Show Package where
    show (Package a b c d e) = unwords ["#" ++ show a,b,c,d,e]

instance Show Module where
    show (Module a b c) = unwords ["#" ++ show a, showModule b,
        "{" ++ show c, packageName (lookupVal c) ++ "}"]

instance Show Entry where
    show (Entry a b c) = unwords ["#" ++ show a, concatMap f c, m]
        where
            m = case b of
                    Nothing -> ""
                    Just y -> "{" ++ show y ++ " " ++ showModule (moduleName $ lookupVal y) ++ "}"

            f (Keyword x) = x
            f (Text x) = x
            f (Focus x) = x
            f (ArgPos _ x) = x
            f (Result x) = x


