
module Hoogle.DataBase.TypeSearch.Score where

import Hoogle.DataBase.TypeSearch.Cost


data TypeScore = TypeScore
    {typeScoreTotal :: Int
    ,typeScoreCosts :: [Cost]
    }

instance Eq TypeScore where
    a == b = typeScoreTotal a == typeScoreTotal b

instance Ord TypeScore where
    compare a b = compare (typeScoreTotal a) (typeScoreTotal b)

instance Show TypeScore where
    show = show . typeScoreCosts


blankTypeScore = TypeScore 0 []
