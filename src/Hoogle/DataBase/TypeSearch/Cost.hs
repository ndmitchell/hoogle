
module Hoogle.DataBase.TypeSearch.Cost where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import General.Code

type CostScore = Int

data Cost = Cost CostScore CostDetail
            deriving (Show,Ord,Eq)

instance BinaryDefer Cost where
    put (Cost a b) = put a
    get = undefined
    size _ = size (undefined :: CostDetail)
    putFixed (Cost a b) = putFixed b
    getFixed = do
        b <- getFixed
        return $ Cost (costScore b) b


data CostDetail
    = CostAlias String String -- ^ I followed an alias from a to b
    | CostDelArg -- ^ I deleted an argument from the result
    | CostArgReorder -- ^ I reordered some arguments
      deriving (Eq,Ord,Show)

instance BinaryDefer CostDetail where
    put = undefined
    get = undefined




-- transform the costCode to a costScore
costScore :: CostDetail -> CostScore
costScore _ = 1


