
module Hoogle.DataBase.TypeSearch.Score where

import General.Code
import Data.Binary.Defer.Index
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


emptyTypeScore :: TypeScore
emptyTypeScore = TypeScore 0 []


addTypeScore :: Cost -> TypeScore -> TypeScore
addTypeScore c (TypeScore tot cs) = TypeScore (tot + costScore c) (c:cs)


addTypeScores :: [Cost] -> TypeScore -> TypeScore
addTypeScores xs t = foldl (flip addTypeScore) t xs



mergeTypeScores :: [TypeScore] -> TypeScore
mergeTypeScores ts = TypeScore (sum tot) (concat cs)
    where (tot,cs) = unzip $ map (typeScoreTotal &&& typeScoreCosts) ts
