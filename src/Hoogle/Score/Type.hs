
module Hoogle.Score.Type(
    Score, TypeCost(..), TextMatch(..),
    textScore, typeScore,
    scoreCosts, cost
    ) where

import Data.List
import Data.Monoid


data TypeCost
    = CostAliasFwd
    | CostAliasBwd
    | CostUnbox
    | CostRebox
    | CostRestrict
    | CostUnrestrict
    | CostDupVarResult
    | CostDupVarQuery
    | CostInstanceDel
    | CostInstanceAdd
    | CostDeadArg
    | CostArgReorder
      deriving (Show,Eq,Ord,Enum,Bounded)

cost :: TypeCost -> Int
cost CostAliasFwd     =    1  -- 1..1000
cost CostAliasBwd     =    1  -- 1..997
cost CostUnbox        =    5  -- 5..1000
cost CostRebox        =    4  -- 4..999
cost CostRestrict     =    5  -- 5..1000
cost CostUnrestrict   =    4  -- 4..1000
cost CostDupVarResult =    4  -- 4..999
cost CostDupVarQuery  =    5  -- 5..1000
cost CostInstanceDel  =    4  -- 4..999
cost CostInstanceAdd  =    4  -- 4..999
cost CostDeadArg      =    3  -- 3..998
cost CostArgReorder   =    1  -- 1..1000


data TextMatch
    = MatchExact
    | MatchPrefix
    | MatchSubstr
      deriving (Show,Eq,Ord,Enum,Bounded)


-- | A score, representing how close a match is. Lower scores are better.
data Score = Score Int [TypeCost] [TextMatch]

instance Monoid Score where
    mempty = Score 0 [] []
    mappend (Score x1 x2 x3) (Score y1 y2 y3) = Score (x1+y1) (sort $ x2++y2) (sort $ x3++y3)

textScore :: TextMatch -> Score
textScore x = Score 0 [] [x]

typeScore :: [TypeCost] -> Score
typeScore xs = Score (sum $ map cost xs) (sort xs) []

scoreCosts :: Score -> [TypeCost]
scoreCosts (Score _ x _) = x


instance Show Score where
    show (Score _ a b) = intercalate "+" $ map (drop 4 . show) a ++ map (drop 5 . show) b

instance Eq Score where
    Score x1 x2 x3 == Score y1 y2 y3 = x1 == y1 && x3 == y3

instance Ord Score where
    compare (Score x1 x2 x3) (Score y1 y2 y3) = compare (x1,x3) (y1,y3)
