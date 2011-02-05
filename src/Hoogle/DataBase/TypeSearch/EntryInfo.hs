{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.TypeSearch.EntryInfo where

import Hoogle.Store.All
import Hoogle.Store.Index
import Hoogle.Type.All
import Data.Typeable


-- the information about an entry, including the arity
data EntryInfo = EntryInfo
    {entryInfoEntries :: [Link Entry]
    ,entryInfoArity :: Int
    ,entryInfoContext :: TypeContext
    ,entryInfoAlias :: [String]
    } deriving (Eq,Show,Typeable)

instance Ord EntryInfo where
    compare (EntryInfo [] x1 x2 x3) (EntryInfo [] y1 y2 y3) = compare (x1,x2,x3) (y1,y2,y3)
    compare _ _ = error "Ord EntryInfo, can't compare EntryInfo's with items in them"

instance BinaryDefer EntryInfo where
    put (EntryInfo a b c d) = put4 a b c d
    get = get4 EntryInfo
