
module Hoogle.DataBase.TypeSearch.Cost(
    Cost(..), reverseCost
    ) where

import Data.Binary.Defer
import General.Code


data Cost
    = CostReverse Cost -- ^ Reverse/Reverse is illegal
    -- standard
    | CostAlias String -- ^ a, where a |-> alias a
    | CostUnbox String -- ^ M, where M a |-> a, "" for a variable
    | CostRestrict String String -- ^ M a, where M |-> a
      deriving (Eq,Ord,Show)


instance BinaryDefer Cost where
    put (CostReverse a)    = putByte 0 >> put1 a
    put (CostAlias a)      = putByte 1 >> put1 a
    put (CostUnbox a)      = putByte 2 >> put1 a
    put (CostRestrict a b) = putByte 3 >> put2 a b

    get = do
        i <- getByte
        case i of
            0 -> get1 CostReverse
            1 -> get1 CostAlias
            2 -> get1 CostUnbox
            3 -> get2 CostRestrict


reverseCost :: Cost -> Maybe Cost
reverseCost (CostReverse _) = Nothing
reverseCost x = Just $ CostReverse x
