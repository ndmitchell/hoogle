{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.Type(module Hoogle.DataBase.Type, module X) where

import Hoogle.DataBase.Items           as X
import Hoogle.DataBase.Suggest         as X
import Hoogle.DataBase.Aliases         as X
import Hoogle.DataBase.Instances       as X
import Hoogle.DataBase.SubstrSearch    as X
import Hoogle.DataBase.TypeSearch.All  as X
import Hoogle.Store.All
import Hoogle.Type.All
import General.Base


-- suggest, aliases and instances are used for linking with packages
-- that depend on this database
data DataBase = DataBase
    {items :: Items
    ,nameSearch :: SubstrSearch (Once Entry)
    ,typeSearch :: TypeSearch
    ,suggest :: Suggest
    ,aliases :: Aliases
    ,instances :: Instances
    }
    deriving Typeable

instance NFData DataBase where
    rnf (DataBase a b c d e f) = rnf (a,b,c,d,e,f)

instance Store DataBase where
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

