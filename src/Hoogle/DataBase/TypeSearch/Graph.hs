{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph(
    Graph, newGraph,
    graphSearch
    ) where

import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.TypeScore
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.Result
import Hoogle.DataBase.Aliases
import Hoogle.Item.All
import Hoogle.TypeSig.All
import Data.Generics.Uniplate
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Data.Threshold
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import General.Code
import Data.Key
import qualified Data.Binary.Defer.Graph as G
import Data.Binary.Defer.Graph hiding (Graph, Graph_)



data Graph = Graph (Map.Map Type GraphNode)
                   (G.Graph Node Edge)

data Node = Node (Link EntryInfo) ArgPos Binding
            deriving Show

data Edge = Edge Cost UniqueBinding
            deriving Show



instance Show Graph where
    show (Graph ns g) = showGraphWith (\i -> show $ mp IntMap.! i) g
        where mp = IntMap.fromList $ map swap $ Map.toList ns


instance BinaryDefer Graph where
    put (Graph a b) = put2 a b
    get = get2 Graph

instance BinaryDefer Node where
    put (Node a b c) = put3 a b c
    get = get3 Node

instance BinaryDefer Edge where
    put (Edge a b) = put2 a b
    get = get2 Edge


---------------------------------------------------------------------
-- GRAPH CONSTRUCTION

type Graph_ = G.Graph_ Type Node Edge


newGraph :: Aliases -> [(Link EntryInfo, ArgPos, Type)] -> Graph
newGraph as xs = Graph mp g
    where (g,mp) = graphFreeze $ reverseLinks $ populateGraph as $ initialGraph xs


-- create the initial graph
initialGraph :: [(Link EntryInfo, ArgPos, Type)] -> Graph_ 
initialGraph xs = newGraph_{graphResults = map newGraphResult xs}


-- create a result, and figure out what the relative is
newGraphResult :: (Link EntryInfo, ArgPos, Type) -> (Type, Node)
newGraphResult (e,p,t) = (t2, Node e p (reverseBinding bind))
    where (bind,t2) = alphaFlatten t


-- add links between each step
populateGraph :: Aliases -> Graph_ -> Graph_
populateGraph as = graphFollow (followNode as)


-- follow a node to all possible next steps
-- all next ones must have already been alpha flattened
--
-- follow:
--  * Unboxing:     m a |-> a, M a |-> a
--  * Restriction:  M |-> _a
--  * Alias:        a |-> alias(a)
--
-- All created variables should be "_", but alphaFlatten will
-- remove these.
followNode :: Aliases -> Type -> [(Type, Edge)]
followNode as t | length (universe t) > 5 = []
                | otherwise =
        [(t3, Edge cost $ optimiseUniqueBinding bind)
        | (cost,t2) <- next, let (bind,t3) = alphaFlatten t2]
    where
        next = restrict t ++ unbox t ++ alias as t


restrict :: Type -> [(Cost, Type)]
restrict t = [(CostRestrict a "_", gen $ TVar "_") | (TLit a, gen) <- contexts t]


-- nub because consider: [[a]], two unboxes get you to the same place
unbox :: Type -> [(Cost, Type)]
unbox t = nub [(CostUnbox a, gen b) | (TApp x [b], gen) <- contexts t, a <- f x]
    where f (TLit a) = [a]
          f (TVar a) = [""]
          f _ = []


alias :: Aliases -> Type -> [(Cost, Type)]
alias as t = map (CostAlias *** id) $ followAliases as t


-- add reverse links where you can, i.e. aliases
reverseLinks :: Graph_ -> Graph_
reverseLinks g = g{graphEdges = graphEdges g ++ mapMaybe f (graphEdges g)}
    where
        f (k1,k2,Edge c b) = do
            c <- reverseCost c
            return (k2,k1,Edge c (reverseBinding b))


---------------------------------------------------------------------
-- GRAPH SEARCHING

-- must search for each (node,bindings) pair, rather than just nodes

graphSearch :: Aliases -> Graph -> Type -> [ResultArg]
graphSearch as g@(Graph _ gg) t
        | isNothing node = error $ "Couldn't find a start spot for: " ++ show t -- []
        | otherwise = threshold $ concat [[Threshold c, Result s (ResultArg e a s)]
            | (c,Node e a b) <- searchDijkstraCycle (emptyTypeScore bind) step (fromJust node) gg
            , Just s <- [scoreBinding b c]]
    where
        (bind,t2) = alphaFlatten t
        node = graphStart as g t2

        step :: Edge -> TypeScore -> Maybe TypeScore
        step (Edge c b) = liftM (scoreUniqueBinding b) . addCost c


-- TODO: Find a better starting place
graphStart :: Aliases -> Graph -> Type -> Maybe GraphNode
graphStart as g t = msum $ map (graphFind g) [t]


graphFind :: Graph -> Type -> Maybe GraphNode
graphFind (Graph mp _) t = Map.lookup t mp
