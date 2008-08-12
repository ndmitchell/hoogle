
module Hoogle.DataBase.TypeSearch.Cost(
    Cost(..), cost, score, (*+)
    ) where


infixl 7  *+ -- same as *

(*+) :: Cost -> Int -> [Cost]
(*+) = flip replicate


data Cost = CostAliasFwd
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


score :: [Cost] -> Int
score = sum . map cost


cost :: Cost -> Int
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
