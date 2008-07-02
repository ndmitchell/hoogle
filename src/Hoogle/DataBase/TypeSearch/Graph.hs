{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph where

import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Instances
import Hoogle.DataBase.TypeSearch.Aliases
import Hoogle.DataBase.Item
import Hoogle.TypeSig.All
import Data.Binary.Defer.Index


type ArgPos = Int
type Binding = [(String,String)]


data Graph = Graph (Index Node)

-- AnswerArg.TypeScore is invalid within a node
data Node = Node [AnswerArg] [(Lookup Node, Lookup Cost)]

data AnswerArg = AnswerArg (Lookup Entry) ArgPos [Binding] TypeScore



---------------------------------------------------------------------
-- GRAPH CONSTRUCTION

graph :: Instances -> Aliases -> [(Lookup Entry, ArgPos, TypeSig)] -> IndexMutable Cost -> (IndexMutable Cost, Graph)
graph is as = undefined


---------------------------------------------------------------------
-- GRAPH SEARCHING

data GraphSearch = GraphSearch


graphSearch :: Index Cost -> Graph -> TypeSig -> (GraphSearch, [AnswerArg])
graphSearch = undefined


graphFollow :: Lookup Cost -> GraphSearch -> (GraphSearch, [AnswerArg])
graphFollow = undefined


graphNext :: GraphSearch -> Maybe (Lookup Cost, CostScore)
graphNext = undefined
