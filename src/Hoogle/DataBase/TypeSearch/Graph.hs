{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph(
    Graph, newGraph,
    GraphResult(..), ArgPos, Binding,
    graphSearch
    ) where

import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.TypeSearch.Score
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
import Control.Monad.State hiding (put,get)
import qualified Control.Monad.State as S



type ArgPos = Int
type Binding = [(String,String)]

-- first argument is a list of contexts, (Context,Variable)
type TypeContext = [(String,String)]
data TypePair = TypePair TypeContext Type
                deriving (Eq,Ord)

instance Show TypePair where
    show (TypePair c t) = show $ TypeSig [TApp (TLit a) [TVar b] | (a,b) <- c] t


data Graph = Graph (Map.Map Type [(TypeContext, GraphNode)])
                   (G.Graph GraphResult (Link Cost, Binding))


instance Show Graph where
    show (Graph ns g) = showGraphWith (\i -> show $ mp IntMap.! i) g
        where mp = IntMap.fromList [(n, TypePair c t) | (t,as) <- Map.toList ns, (c,n) <- as]


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
    get = get3 (\a b c -> GraphResult a b c blankTypeScore)

instance Show GraphResult where
    show (GraphResult a b c d) = '#':show (linkKey a) ++ '.':show b ++ c2
        where c2 = if null c then "" else show c

---------------------------------------------------------------------
-- GRAPH CONSTRUCTION

type Graph_ = G.Graph_ TypePair GraphResult (Cost, Binding)


newGraph :: Aliases -> Instances -> [(Link Entry, ArgPos, TypeSig)] ->
            Index_ Cost -> (Index_ Cost, Graph)
newGraph as is xs cost = (cost2, Graph mp2 g)
    where
        g_ = reverseLinks $ populateGraph as is $ initialGraph is xs
        (cost2,g_2) = linkCosts g_ cost
        (g,mp) = graphFreeze g_2
        mp2 = fromListMany [(t,(c,v)) | (TypePair c t,v) <- Map.toList mp]

fromListMany :: Ord k => [(k,v)] -> Map.Map k [v]
fromListMany = Map.fromAscList . map (fst . head &&& map snd) . groupFst . sortFst


linkCosts :: Graph_ -> Index_ Cost -> (Index_ Cost, G.Graph_ TypePair GraphResult (Link Cost, Binding))
linkCosts (G.Graph_ res edges) costs = (costs2, G.Graph_ res edges2)
    where
        (costs2,edges2) = mapAccumR f costs edges

        f costs (k1,k2,(c,b)) = (costs2, (k1,k2,(c2,b)))
            where (costs2,c2) = getLink c costs


-- create the initial graph
initialGraph :: Instances -> [(Link Entry, ArgPos, TypeSig)] -> Graph_ 
initialGraph is xs = newGraph_{graphResults = map (newGraphResult is) xs}


-- create a result, and figure out what the relative is
newGraphResult :: Instances -> (Link Entry, ArgPos, TypeSig) -> (TypePair, GraphResult)
newGraphResult is (e,p,t) = (tp, GraphResult e p bind blankTypeScore)
    where (bind,tp) = alphaFlatten $ contextNorm is t


-- add links between each step
populateGraph :: Aliases -> Instances -> Graph_ -> Graph_
populateGraph as is = graphFollow (followNode as is)


-- follow a node to all possible next steps
-- all next ones must have already been alpha flattened
--
-- follow:
--  * Unboxing:     m a |-> a, M a |-> a
--  * Restriction:  M |-> a
--  * Alias:        a |-> alias(a)
--  * Context:      C a => a |-> a
--  * Membership:   C M => M |-> C a => a
followNode :: Aliases -> Instances -> TypePair -> [(TypePair, (Cost, Binding))]
followNode as is (TypePair con t) =
        -- TODO: Should do something sensible with bindings
        nub [(snd $ alphaFlatten a, (newCost b, c)) | (a,b,c) <- next]
    where
        next = cont unbox ++ restrict ++ cont alias -- TODO: Context and Membership
        free = map (:[]) ['a'..] \\ [v | TVar v <- universe t]
        cont f = concatMap f $ contexts t

        unbox (TApp (TLit a) [b], gen) = [(TypePair con (gen b), CostUnbox a, [])]
        unbox (TApp (TVar a) [b], gen) = [(TypePair con (gen b), CostUnbox "", [])]
        unbox _ = []

        -- Only restrict things of kind *
        restrict = [(TypePair con a, CostRestrict b, []) | (a,b) <- f t]
            where fs xs = [(b c, d) | (a,b) <- xs, (c,d) <- f a]
                  f (TLit a) = [(TVar $ head free, a)]
                  f x@TApp{} = fs $ tail $ holes x
                  f x = fs $ holes x

        alias (a,gen) = [(TypePair con (gen b), CostAlias name, []) | Just (name,b) <- [followAliases as a]]


-- add reverse links where you can, i.e. aliases
reverseLinks :: Graph_ -> Graph_
reverseLinks g = g{graphEdges = graphEdges g ++ mapMaybe f (graphEdges g)}
    where
        f (k1,k2,(c,b)) = do
            c <- reverseCost c
            return (k2,k1,(c,reverseBinding b))


reverseBinding xs = [(b,a) | (a,b) <- xs]


-- normalise the letters in a type, so that:
-- each variable is distinct
-- the context is in order
-- all context relates to free variables
-- binding is original |-> new
alphaFlatten :: TypePair -> (Binding,TypePair)
alphaFlatten (TypePair a b) = (sort bind, TypePair a2 b2)
    where
        a2 = nub $ sort $ concatMap g a
        (b2,(bind,_)) = runState (transformM f b) ([], map (:[]) ['a'..])

        f (TVar x) = do
            (bind,v:vs) <- S.get
            S.put ((x,v):bind,vs)
            return $ TVar v
        f x = return x

        g (cls,v) = [(cls,b) | (a,b) <- bind, a == v]


-- disguard any context which relates to variables not in the type
-- convert using whatever scheme the Instances say
contextNorm :: Instances -> TypeSig -> TypePair
contextNorm is t = TypePair [(x,y) | TApp (TLit x) [TVar y] <- a, x `elem` vs] b
    where
        (TypeSig a b) = normContext is t
        vs = [v | TVar v <- universe b]


---------------------------------------------------------------------
-- GRAPH SEARCHING

-- must search for each (node,bindings) pair, rather than just nodes

graphSearch :: Aliases -> Instances -> Graph -> TypeSig -> [GraphResult]
graphSearch as is g@(Graph _ gg) t
        | isNothing node = error $ "Couldn't find a start spot for: " ++ show t -- []
        | otherwise = [r{graphResultScore=s}
            | (s,b,r) <- searchDijkstraState (blankTypeScore, []) step (fromJust node) gg]
    where
        (bind,t2) = alphaFlatten $ contextNorm is t
        node = graphStart as is g t2

        step :: (Link Cost, Binding) -> (TypeScore, Binding) -> (TypeScore, Binding)
        step (cost,_) (score,_) = (addTypeScore cost score, [])


-- TODO: Find a better starting place
graphStart :: Aliases -> Instances -> Graph -> TypePair -> Maybe GraphNode
graphStart as is g t = msum $ map (graphFind g) [t]


graphFind :: Graph -> TypePair -> Maybe GraphNode
graphFind (Graph mp _) (TypePair c t) = do
    r <- Map.lookup t mp
    return $ snd $ head $ sortWith f r
    where
        -- ideally equal, otherwise one with fewer contexts
        f (c2,_) | null more = Left (length less)
                 | otherwise = Right (length more)
            where more = c2 \\ c    
                  less = c  \\ c2
