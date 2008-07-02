
module Hoogle.DataBase.TypeSearch.Cost where


data Cost = Cost
    {costId :: Int      -- the Id of the Cost item
    ,costCode :: Int    -- the code of the item
    ,costStr :: String  -- text describing the penalty
    } deriving Show


-- transform the costCode to a costScore
costScore :: Cost -> Int
costScore = undefined


