{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph(
    Graph, newGraph,
    GraphResult(..), ArgPos, Binding,
    GraphSearch, graphSearch, graphFound, graphFollow, graphCost, graphNext
    ) where

import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.Item
import Hoogle.TypeSig.All
import Data.Generics.Uniplate
import Data.Binary.Defer.Index
import qualified Data.Map as Map
import Control.Monad.State
import General.Code


type ArgPos = Int
type Binding = [(String,String)]

-- first argument is a list of contexts, (Context,Variable)
type TypeContext = [(String,String)]
data TypePair = TypePair TypeContext Type


data Graph = Graph (Map.Map Type [(TypeContext, Lookup Node)]) (Index Node)

data Node = Node [GraphResult] [(Lookup Node, Lookup Cost)]

-- GraphResult.TypeScore is invalid within a node
-- is not saved, is loaded as blank
data GraphResult = GraphResult
    {graphResultEntry :: Lookup Entry
    ,graphResultPos :: ArgPos
    ,graphResultBinding :: [Binding]
    ,graphResultScore :: TypeScore
    }


---------------------------------------------------------------------
-- GRAPH CONSTRUCTION

{-
Data:

1) Map TypeSig (Lookup Node, Node)

The graph as it currently stands, with each node indexed by
a TypeSig

2) Set TypeSig

Those nodes in the graph which have not yet been explored
-}

data S = S
    {costs :: IndexMutable Cost
    ,graph :: Map.Map TypePair (Lookup Node, Node)
    }


newGraph :: Aliases -> Instances -> [(Lookup Entry, ArgPos, TypeSig)] -> IndexMutable Cost -> (IndexMutable Cost, Graph)
newGraph as is xs cost = (costs sN, f (graph sN))
    where
        sN = execState (initialGraph xs >> populateGraph >> reverseLinks) s0
        s0 = S cost Map.empty

        f mp = Graph
            (fromListMany [(t,(c,v)) | (TypePair c t,(v,_)) <- Map.toList mp])
            (newIndex $ map snd $ sortFst $ Map.elems mp)


fromListMany :: Ord k => [(k,v)] -> Map.Map k [v]
fromListMany = Map.fromList . map (fst . head &&& map snd) . groupFst . sortFst


-- create the initial graph
initialGraph :: [(Lookup Entry, ArgPos, TypeSig)] -> State S ()
initialGraph = undefined


-- add links between each step
populateGraph :: State S ()
populateGraph = undefined


-- add reverse links where you can, i.e. aliases
reverseLinks :: State S ()
reverseLinks = undefined


-- normalise the letters in a type, so that:
-- each variable is distinct
-- the context is in order
-- all context relates to free variables
-- binding is original |-> new
alphaFlatten :: TypePair -> (Binding,TypePair)
alphaFlatten (TypePair a b) = (bind, TypePair a2 b2)
    where
        a2 = nub $ sort $ concatMap g a
        (b2,(bind,_)) = runState (transformM f b) ([], map (:[]) ['a'..])

        f (TVar x) = do
            (bind,v:vs) <- get
            put ((x,v):bind,vs)
            return $ TVar v
        f x = return x

        g (cls,v) = [(cls,b) | (a,b) <- bind, a == v]


-- disguard any context which relates to variables not in the type
-- convert using whatever scheme the Instances say
contextNorm :: Instances -> TypeSig -> TypePair
contextNorm is t = TypePair [(x,y) | TApp (TLit x) [TVar y] <- a] b
    where (TypeSig a b) = normContext is t


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
