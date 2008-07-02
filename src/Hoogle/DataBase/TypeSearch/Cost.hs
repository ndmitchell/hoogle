
module Hoogle.DataBase.TypeSearch.Cost where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import General.Code


type CostScore = Int

newtype Costs = Costs (Index (CostScore, Cost))

instance BinaryDefer Costs where
    put (Costs xs) = put $ fmap snd xs
    get = liftM (Costs . fmap (costScore &&& id)) get


data Cost = CostAlias String String -- ^ I followed an alias from a to b
          | CostDelArg -- ^ I deleted an argument from the result
            deriving (Eq,Ord,Show)

instance BinaryDefer Cost where
    put = undefined
    get = undefined






-- transform the costCode to a costScore
costScore :: Cost -> CostScore
costScore _ = 1


