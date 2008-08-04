
module Hoogle.DataBase.TypeSearch.Score(
    Score(..), cost, score, (*+)
    ) where


infixl 7  *+ -- same as *

(*+) :: Score -> Int -> [Score]
(*+) = flip replicate


data Score = ScoreAliasFwd
           | ScoreAliasBwd
           | ScoreUnbox
           | ScoreRebox
           | ScoreRestrict
           | ScoreUnrestrict
           | ScoreDupVarResult
           | ScoreDupVarQuery
           | ScoreInstanceDel
           | ScoreInstanceAdd
           | ScoreDeadArg


score :: [Score] -> Int
score = sum . map cost


cost :: Score -> Int
cost _ = 1
