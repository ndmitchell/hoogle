
module Hoogle.DataBase.TypeSearch.Cost where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import General.Code

type CostScore = Int

data Cost = Cost CostScore CostDetail
            deriving (Show,Ord,Eq)

instance BinaryDefer Cost where
    put (Cost a b) = put a
    get = get1 newCost
    size _ = size (undefined :: CostDetail)
    putFixed (Cost a b) = putFixed b
    getFixed = getFixed1 newCost


data CostDetail
    = CostAlias String String -- ^ I followed an alias from a to b
    | CostDelArg -- ^ I deleted an argument from the result
    | CostArgReorder -- ^ I reordered some arguments
      deriving (Eq,Ord,Show)

instance BinaryDefer CostDetail where
    put = error "BinaryDefer.CostDetail.put"
    get = error "BinaryDefer.CostDetail.get"




-- transform the costCode to a costScore
newCost :: CostDetail -> Cost
newCost xs = Cost 1 xs


