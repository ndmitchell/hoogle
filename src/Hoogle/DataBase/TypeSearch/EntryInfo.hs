{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.TypeSearch.EntryInfo where

import Hoogle.Store.All
import Hoogle.Type.All
import General.Base


-- the information about an entry, including the arity
data EntryInfo = EntryInfo
    {entryInfoKey :: Int -- allow cheap equality
    ,entryInfoEntries :: [Once Entry]
    ,entryInfoArity :: Int
    ,entryInfoContext :: TypeContext
    ,entryInfoAlias :: [String]
    } deriving (Show,Typeable)

instance NFData EntryInfo where
    rnf (EntryInfo a b c d e) = rnf (a,b,c,d,e)

instance Ord EntryInfo where
    compare (EntryInfo _ [] x1 x2 x3) (EntryInfo _ [] y1 y2 y3) = compare (x1,x2,x3) (y1,y2,y3)
    compare _ _ = error "Ord EntryInfo, can't compare EntryInfo's with items in them"

instance Eq EntryInfo where
    EntryInfo _ [] x1 x2 x3 == EntryInfo _ [] y1 y2 y3 = (x1,x2,x3) == (y1,y2,y3)
    _ == _ = error "Eq EntryInfo, can't compare EntryInfo's with items in them"

instance Store EntryInfo where
    put (EntryInfo a b c d e) = put5 a b c d e
    get = get5 EntryInfo
