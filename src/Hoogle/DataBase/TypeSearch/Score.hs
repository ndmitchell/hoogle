
module Hoogle.DataBase.TypeSearch.Score where

import General.Code
import Data.Binary.Defer.Index
import Hoogle.DataBase.TypeSearch.Cost


data TypeScore = TypeScore
    {typeScoreTotal :: Int
    ,typeScoreCosts :: [Link Cost]
    }

instance Eq TypeScore where
    a == b = typeScoreTotal a == typeScoreTotal b

instance Ord TypeScore where
    compare a b = compare (typeScoreTotal a) (typeScoreTotal b)

instance Show TypeScore where
    show = show . map fromLink . typeScoreCosts


blankTypeScore :: TypeScore
blankTypeScore = TypeScore 0 []


addTypeScore :: Link Cost -> TypeScore -> TypeScore
addTypeScore c t@(TypeScore total costs)
    | linkKey c `elem` map linkKey costs = t
    | otherwise = TypeScore (total + costScore (fromLink c))
                            (insertBy (compare `on` linkKey) c costs)


-- add custom cost's which should not be nub'd
addTypeScoreDirect :: [Cost] -> TypeScore -> TypeScore
addTypeScoreDirect cs (TypeScore total costs) =
    TypeScore (total + sum (map costScore cs)) (map (newLink 0) cs ++ costs)


mergeTypeScores :: [TypeScore] -> TypeScore
mergeTypeScores ts = TypeScore (sum $ map (costScore . fromLink) cs) cs
    where cs = nubBy ((==) `on` linkKey) $ sortBy (compare `on` linkKey) $ concatMap typeScoreCosts ts
