
module Hoogle.DataBase.Type
    (module Hoogle.DataBase.Type
    ,module Hoogle.DataBase.Items
    ,module Hoogle.DataBase.Suggest
    ,module Hoogle.DataBase.Aliases
    ,module Hoogle.DataBase.Instances
    ,module Hoogle.DataBase.SubstrSearch
    ,module Hoogle.DataBase.TypeSearch.All
    ) where

import Hoogle.DataBase.Items
import Hoogle.DataBase.Suggest
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.Instances
import Hoogle.DataBase.TypeSearch.All
import Hoogle.DataBase.SubstrSearch
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.Type.All
import General.Code


-- suggest, aliases and instances are used for linking with packages
-- that depend on this database
data DataBase = DataBase
    {items :: Items
    ,nameSearch :: SubstrSearch (Link Entry)
    ,typeSearch :: TypeSearch
    ,suggest :: Suggest
    ,aliases :: Aliases
    ,instances :: Instances
    }


instance BinaryDefer DataBase where
    put (DataBase a b c d e f) = put6 a b c d e f
    get = get6 DataBase


instance Show DataBase where
    show = concatMap snd . showDataBaseParts


showDataBaseParts :: DataBase -> [(String,String)]
showDataBaseParts (DataBase a b c d e f) =
    let name * val = (name, "= " ++ name ++ " =\n\n" ++ show val ++ "\n") in
    ["Items" * a,"NameSearch" * b, "TypeSearch" * c
    ,"Suggest" * d, "Aliases" * e, "Instances" * f]


showDataBase :: String -> DataBase -> String
showDataBase "" d = show d
showDataBase x d | null r = "Error: Unknown database part, " ++ x
                 | length r > 1 = "Error: Ambiguous database part, " ++ x
                 | otherwise = head r
    where r = [b | (a,b) <- showDataBaseParts d, lower x `isPrefixOf` lower a]

