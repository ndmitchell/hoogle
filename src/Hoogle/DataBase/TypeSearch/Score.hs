
module Hoogle.DataBase.TypeSearch.Score where

-- Oh, wow, my first _ever_ use of default!
default(Int)

-- scores and their costs

scoreAliasFwd = 1
scoreAliasBwd = 1
scoreUnbox = 1
scoreRebox = 1
scoreRestrict = 1
scoreUnrestrict = 1
scoreDupVarResult = 1
scoreDupVarQuery = 1
scoreInstanceDel = 1
scoreInstanceAdd = 1
scoreDeadArg = 1
