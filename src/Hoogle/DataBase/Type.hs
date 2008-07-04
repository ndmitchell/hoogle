
module Hoogle.DataBase.Type
    (module Hoogle.DataBase.Type
    ,module Hoogle.DataBase.Item
    ,module Hoogle.DataBase.Items
    ,module Hoogle.DataBase.Suggest
    ,module Hoogle.DataBase.Aliases
    ,module Hoogle.DataBase.Instances
    ,module Hoogle.DataBase.TypeSearch.All
    ,module Hoogle.DataBase.NameSearch
    ) where

import Hoogle.DataBase.Item
import Hoogle.DataBase.Items
import Hoogle.DataBase.Suggest
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.Instances
import Hoogle.DataBase.TypeSearch.All
import Hoogle.DataBase.NameSearch
import Data.Binary.Defer


data DataBase = DataBase
    {items :: Items
    ,nameSearch :: NameSearch
    ,typeSearch :: TypeSearch
    ,suggest :: Suggest
    -- aliases and instances are stored
    -- but only used for linking with packages that depend
    -- on this database
    ,aliases :: Aliases
    ,instances :: Instances
    }


instance BinaryDefer DataBase where
    put (DataBase a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
    get = get6 DataBase


instance Show DataBase where
    show (DataBase a b c d e f) =
        z "Items" a ++ z "NameSearch" b ++ z "TypeSearch" c ++
        z "Suggest" d ++ z "Aliases" e ++ z "Instances" f
        where
            z header x = "= " ++ header ++ " =\n\n" ++ show x ++ "\n\n"
