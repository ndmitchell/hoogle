
module Hoogle.DataBase.TypeSearch.Result where

import Hoogle.DataBase.TypeSearch.Score
import Data.Binary.Defer.Index
import Hoogle.TypeSig.All
import Hoogle.Item.All


type ArgPos = Int


-- the return from searching a graph
type Result = (Link Entry,[EntryView],TypeScore)


-- the information about an entry, including the arity
data EntryInfo = EntryInfo (Link Entry) Int TypeContext


-- the result information from a whole type (many ResultArg)
data ResultAll = ResultAll EntryInfo [[ResultArg]]


-- the result information from one single type graph (argument/result)
-- this result points at entry.id, argument, with such a score
data ResultArg = ResultArg (Link EntryInfo) ArgPos TypeScore



addResultAll :: (Maybe ArgPos, ResultArg) -> ResultAll -> (ResultAll, [Result])
addResultAll = undefined


newResultAll :: EntryInfo -> ResultAll
newResultAll = undefined

