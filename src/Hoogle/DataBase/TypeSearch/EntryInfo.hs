
module Hoogle.DataBase.TypeSearch.EntryInfo where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.TypeSig.All
import Hoogle.Item.All
import Data.Typeable


-- the information about an entry, including the arity
data EntryInfo = EntryInfo
    {entryInfoEntries :: [Link Entry]
    ,entryInfoArity :: Int
    ,entryInfoContext :: TypeContext
    } deriving Show


typename_EntryInfo = mkTyCon "Hoogle.DataBase.TypeSearch.Result.EntryInfo"
instance Typeable EntryInfo
    where typeOf _ = mkTyConApp typename_EntryInfo []

instance BinaryDefer EntryInfo where
    put (EntryInfo a b c) = put3 a b c
    get = get3 EntryInfo
