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
data Node = Node [GraphResult] [(Lookup Node, Lookup Cost)]

data GraphResult = GraphResult
    {graphResultEntry :: Lookup Entry
    ,graphResultPos :: ArgPos
    ,graphResultBinding :: [Binding]
    ,graphResultScore :: TypeScore
    }


---------------------------------------------------------------------
-- GRAPH CONSTRUCTION

newGraph :: Instances -> Aliases -> [(Lookup Entry, ArgPos, TypeSig)] -> IndexMutable Cost -> (IndexMutable Cost, Graph)
newGraph is as = undefined


---------------------------------------------------------------------
-- GRAPH SEARCHING

data GraphSearch = GraphSearch


graphSearch :: Index Cost -> Graph -> TypeSig -> GraphSearch
graphSearch = undefined


-- those entires which are at a newly discovered node
graphFound :: GraphSearch -> [GraphResult]
graphFound = undefined


-- follow a graph along a cost edge, that is now free
graphFollow :: Lookup Cost -> GraphSearch -> GraphSearch
graphFollow = undefined


-- what is the minimum cost any future graphFound result may have
graphCost :: GraphSearch -> CostScore
graphCost = undefined


-- ask what possible node could be followed next
graphNext :: GraphSearch -> Maybe (Lookup Cost, Cost)
graphNext = undefined
