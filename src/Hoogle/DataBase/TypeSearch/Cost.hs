
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
            deriving (Show,Eq,Ord,Enum,Bounded)


score :: [Cost] -> Int
score = sum . map cost


cost :: Cost -> Int
cost _ = 1
