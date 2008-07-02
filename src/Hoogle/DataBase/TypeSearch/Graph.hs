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


data Graph = Graph

data GraphSearch = GraphSearch

data AnswerArg = AnswerArg (Lookup Entry) ArgPos [Binding] TypeScore


type ArgPos = Int
type Binding = [(String,String)]


graph :: Instances -> Aliases -> [(Lookup Entry, ArgPos, TypeSig)] -> Graph
graph = undefined


graphSearch :: Graph -> TypeSig -> (GraphSearch, [AnswerArg])
graphSearch = undefined


graphFollow :: Lookup Cost -> GraphSearch -> (GraphSearch, [AnswerArg])
graphFollow = undefined


graphNext :: GraphSearch -> Maybe (Lookup Cost)
graphNext = undefined
