
module Data.Binary.Defer.Graph(
    Graph, GraphNode, showGraphWith,
    searchDijkstraState,
    Graph_(..), newGraph_,
    graphFreeze, graphFollow
    ) where

import Data.Binary.Defer
import Data.Binary.Defer.Array
import Data.List
import qualified Data.Heap as Heap
import qualified Data.Map as Map
import qualified Data.Set as Set


---------------------------------------------------------------------
-- GRAPH

data Graph n e = Graph (Array (Node n e))

data Node n e = Node {nodeResults :: [n], nodeEdges :: [(e, GraphNode)]}

type GraphNode = Int


showGraphWith :: (Show n, Show e) => (GraphNode -> String) -> Graph n e -> String
showGraphWith showNode (Graph xs) = unlines $ concat $ zipWith f [0..] $ elems xs
    where
        f i (Node res es) = showNode i : map ("    "++) (r : map g es)
            where r = if null res then "No results" else unwords (map show res)

        g (e,n) = showNode n ++ " ==> " ++ show e


instance (Show n, Show e) => Show (Graph n e) where
    show = showGraphWith (\x -> '#':show x)


instance (BinaryDefer n, BinaryDefer e) => BinaryDefer (Graph n e) where
    put (Graph a) = put1 a
    get = get1 Graph

instance (BinaryDefer n, BinaryDefer e) => BinaryDefer (Node n e) where
    put (Node a b) = put2 a b
    get = get2 Node


-- Given an initial (cost,state), and how to transform a (cost,state) along an edge
-- return the (cost,state,node) pairs with the invariants that:
--    Items are returned sorted by c
--    No (_,s,n) pair is returned twice
searchDijkstraState :: (Ord c, Ord s) =>
    (c,s) -> (e -> (c,s) -> (c,s)) ->
    GraphNode -> Graph n e -> [(c,s,n)]
searchDijkstraState (c,s) gen n (Graph xs) = f Set.empty (Heap.singleton c (s,n))
    where
        f seen next = case Heap.pop next of
            Nothing -> []
            Just ((c,(s,n)),next)
                | (s,n) `Set.member` seen -> f seen next
                | otherwise -> [(c,s,n) | n <- ns] ++ f seen2 next2
                    where Node ns es = xs ! n
                          seen2 = Set.insert (s,n) seen
                          next2 = Heap.pushList [(c2,(s2,n)) | (e,n) <- es, let (c2,s2) = gen e (c,s)] next


---------------------------------------------------------------------
-- MUTABLE GRAPH

data Graph_ k n e = Graph_ {graphResults :: [(k,n)], graphEdges :: [(k,k,e)]}


newGraph_ :: Graph_ k n e
newGraph_ = Graph_ [] []


graphKeys :: Graph_ k n e -> [k]
graphKeys (Graph_ res es) = map fst res ++ concatMap (\(a,b,c) -> [a,b]) es


-- follow a node, if you create a new node, keep following
graphFollow :: Ord k => (k -> [(k,e)]) -> Graph_ k n e -> Graph_ k n e
graphFollow next g = g{graphEdges = graphEdges g ++ f Set.empty (graphKeys g)}
    where
        f seen [] = []
        f seen (t:odo)
            | t `Set.member` seen = f seen odo
            | otherwise =
                [(t,k,e) | (k,e) <- nxt] ++ f (Set.insert t seen) (map fst nxt ++ odo)
            where nxt = next t


-- TODO: Profiling shows 28% time and 58% space on database creation is
--       spent in this function
graphFreeze :: Ord k => Graph_ k n e -> (Graph n e, Map.Map k GraphNode)
graphFreeze g = (Graph $ array $ map f ks, mp)
    where
        mp = Map.fromAscList $ zip ks [0..]
        ks = Set.toAscList $ Set.fromList $ graphKeys g

        f k = Node [n | (k1,n) <- graphResults g, k1 == k]
                   [(e, mp Map.! k2) | (k1,k2,e) <- graphEdges g, k1 == k]

