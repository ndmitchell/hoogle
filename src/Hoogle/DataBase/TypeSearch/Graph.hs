{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph(
    Graph, newGraph,
    GraphResult(..), ArgPos,
    graphSearch
    ) where

import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.Item.All
import Hoogle.TypeSig.All
import Data.Generics.Uniplate
import Data.Binary.Defer
import Data.Binary.Defer.Index
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import General.Code
import Data.Key
import qualified Data.Binary.Defer.Graph as G
import Data.Binary.Defer.Graph hiding (Graph, Graph_)



type ArgPos = Int


data Graph = Graph (Map.Map Type [(TypeContext, GraphNode)])
                   (G.Graph GraphResult (Link Cost, Binding))


instance Show Graph where
    show (Graph ns g) = showGraphWith (\i -> show $ mp IntMap.! i) g
        where mp = IntMap.fromList [(n, TypeSimp c t) | (t,as) <- Map.toList ns, (c,n) <- as]


instance BinaryDefer Graph where
    put (Graph a b) = put2 a b
    get = get2 Graph


-- GraphResult.TypeScore is invalid within a node
-- is not saved, is loaded as blank
data GraphResult = GraphResult
    {graphResultEntry :: Link Entry
    ,graphResultPos :: ArgPos
    ,graphResultBinding :: Binding
    ,graphResultScore :: TypeScore
    }

instance BinaryDefer GraphResult where
    put (GraphResult a b c d) = put3 a b c
    get = get3 (\a b c -> GraphResult a b c emptyTypeScore)

instance Show GraphResult where
    show (GraphResult a b c d) = '#':show (linkKey a) ++ '.':show b ++ show c

---------------------------------------------------------------------
-- GRAPH CONSTRUCTION

type Graph_ = G.Graph_ TypeSimp GraphResult (Cost, Binding)


newGraph :: Aliases -> Instances -> [(Link Entry, ArgPos, TypeSig)] ->
            Index_ Cost -> (Index_ Cost, Graph)
newGraph as is xs cost = (cost2, Graph mp2 g)
    where
        g_ = reverseLinks $ populateGraph as is $ initialGraph is xs
        (cost2,g_2) = linkCosts g_ cost
        (g,mp) = graphFreeze g_2
        mp2 = fromListMany [(t,(c,v)) | (TypeSimp c t,v) <- Map.toList mp]


linkCosts :: Graph_ -> Index_ Cost -> (Index_ Cost, G.Graph_ TypeSimp GraphResult (Link Cost, Binding))
linkCosts (G.Graph_ res edges) costs = (costs2, G.Graph_ res edges2)
    where
        (costs2,edges2) = mapAccumR f costs edges

        f costs (k1,k2,(c,b)) = (costs2, (k1,k2,(c2,b)))
            where (costs2,c2) = getLink c costs


-- create the initial graph
initialGraph :: Instances -> [(Link Entry, ArgPos, TypeSig)] -> Graph_ 
initialGraph is xs = newGraph_{graphResults = map (newGraphResult is) xs}


-- create a result, and figure out what the relative is
newGraphResult :: Instances -> (Link Entry, ArgPos, TypeSig) -> (TypeSimp, GraphResult)
newGraphResult is (e,p,t) = (tp, GraphResult e p (reverseBinding bind) emptyTypeScore)
    where (bind,tp) = alphaFlatten $ normContext is t


-- add links between each step
populateGraph :: Aliases -> Instances -> Graph_ -> Graph_
populateGraph as is = graphFollow (followNode as is)


-- follow a node to all possible next steps
-- all next ones must have already been alpha flattened
--
-- follow:
--  * Unboxing:     m a |-> a, M a |-> a
--  * Restriction:  (M :: *) |-> _a
--  * Alias:        a |-> alias(a)
--  * Context:      C a => a |-> a
--  * Membership:   C M => M |-> C _a => _a
--
-- All created variables should be "_a", but alphaFlatten will
-- remove these.
followNode :: Aliases -> Instances -> TypeSimp -> [(TypeSimp, (Cost, Binding))]
followNode as is (TypeSimp con t) =
        nub [(d, (newCost b, c)) | (b,a) <- next, let (c,d) = alphaFlatten a]
    where
        onType c f = map (c *** TypeSimp con) $ f t
        onTypeSimp c f = map (c *** id) $ f $ TypeSimp con t

        next = onType CostUnbox unbox ++
               onType CostRestrict restrict ++
               onTypeSimp CostContext context ++
               onType CostAlias (followAliases as) ++
               onTypeSimp (uncurry CostMember) (followInstances is)

restrict :: Type -> [(String, Type)]
restrict t = [(a, gen $ TVar "_a") |
    (TApp (TLit a) [], gen) <- contexts $ insertTApp t]


unbox :: Type -> [(String, Type)]
unbox t = [(a, gen b) | (TApp x [b], gen) <- contexts t, a <- f x]
    where f (TLit a) = [a]
          f (TVar a) = [""]
          f _ = []


context :: TypeSimp -> [(String, TypeSimp)]
context (TypeSimp con t) = [(e, TypeSimp (pre++post) t)
    | (pre,(e,_):post) <- zip (inits con) (tails con)]


-- add reverse links where you can, i.e. aliases
reverseLinks :: Graph_ -> Graph_
reverseLinks g = g{graphEdges = graphEdges g ++ mapMaybe f (graphEdges g)}
    where
        f (k1,k2,(c,b)) = do
            c <- reverseCost c
            return (k2,k1,(c,reverseBinding b))


---------------------------------------------------------------------
-- GRAPH SEARCHING

-- must search for each (node,bindings) pair, rather than just nodes

graphSearch :: Aliases -> Instances -> Graph -> TypeSig -> [GraphResult]
graphSearch as is g@(Graph _ gg) t
        | isNothing node = error $ "Couldn't find a start spot for: " ++ show t -- []
        | otherwise = [r{graphResultScore=s, graphResultBinding=b `bindCompose` graphResultBinding r}
            | (s,b,r) <- searchDijkstraState (emptyTypeScore, bind) step (fromJust node) gg]
    where
        (bind,t2) = alphaFlatten $ normContext is t
        node = graphStart as is g t2

        step :: (Link Cost, Binding) -> (TypeScore, Binding) -> (TypeScore, Binding)
        step (cost,b1) (score,b2) = (addTypeScore cost score, b2 `bindCompose` b1)


-- TODO: Find a better starting place
graphStart :: Aliases -> Instances -> Graph -> TypeSimp -> Maybe GraphNode
graphStart as is g t = msum $ map (graphFind g) [t]


graphFind :: Graph -> TypeSimp -> Maybe GraphNode
graphFind (Graph mp _) (TypeSimp c t) = do
    r <- Map.lookup t mp
    return $ snd $ head $ sortWith f r
    where
        -- ideally equal, otherwise one with fewer contexts
        f (c2,_) | null more = Left (length less)
                 | otherwise = Right (length more)
            where more = c2 \\ c    
                  less = c  \\ c2
