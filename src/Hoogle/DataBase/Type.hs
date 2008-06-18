
module Hoogle.DataBase.Type
    (module Hoogle.DataBase.Type
    ,module Hoogle.DataBase.Item
    ,module Hoogle.DataBase.Items
    ,module Hoogle.DataBase.Suggest
    ,module Hoogle.DataBase.TypeSearch
    ,module Hoogle.DataBase.NameSearch
    ) where

import Hoogle.DataBase.Item
import Hoogle.DataBase.Items
import Hoogle.DataBase.Suggest
import Hoogle.DataBase.TypeSearch
import Hoogle.DataBase.NameSearch
import Data.Binary.Defer


data DataBase = DataBase
    {items :: Items
    ,nameSearch :: NameSearch
    ,typeSearch :: TypeSearch
    ,suggest :: Suggest
    }


instance BinaryDefer DataBase where
    put (DataBase a b c d) = put a >> put b >> put c >> put d
    get = get4 DataBase


instance Show DataBase where
    show (DataBase a b c d) =
        f "Items" a ++ f "NameSearch" b ++ f "TypeSearch" c ++ f "Suggest" d
        where
            f header x = "= " ++ header ++ " =\n\n" ++ show x ++ "\n\n"
