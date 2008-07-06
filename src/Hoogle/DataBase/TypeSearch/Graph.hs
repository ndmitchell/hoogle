{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph(
    Graph, newGraph, showGraph,
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
import Data.Binary.Defer
import Data.Binary.Defer.Index
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State  hiding (get,put)
import qualified Control.Monad.State as S
import General.Code


type ArgPos = Int
type Binding = [(String,String)]

-- first argument is a list of contexts, (Context,Variable)
type TypeContext = [(String,String)]
data TypePair = TypePair TypeContext Type
                deriving (Eq,Ord)

instance Show TypePair where
    show (TypePair c t) = show $ TypeSig [TApp (TLit a) [TVar b] | (a,b) <- c] t


data Graph = Graph (Map.Map Type [(TypeContext, Lookup Node)]) (Index Node)
             deriving Show

showGraph :: Index Cost -> Graph -> String
showGraph cs (Graph mp ns) = unlines $ concat
        [ f (TypePair b a) (lookupIndex c ns) | (a,bs) <- Map.toList mp, (b,c) <- bs]
    where
        f t (Node res link) = show t : map ("    " ++) (unwords (map h res) : map g link)
        g (ni,ci,b) = show (ni,ci,b)
        h (GraphResult a b c _) = show a ++ "." ++ show b ++ (if null c then "" else show c)

instance BinaryDefer Graph where
    put (Graph a b) = put2 a b
    get = get2 Graph


data Node = Node [GraphResult] [(Lookup Node, Lookup Cost, Binding)]
            deriving Show

instance BinaryDefer Node where
    put (Node a b) = put2 a b
    get = get2 Node


-- GraphResult.TypeScore is invalid within a node
-- is not saved, is loaded as blank
data GraphResult = GraphResult
    {graphResultEntry :: Lookup Entry
    ,graphResultPos :: ArgPos
    ,graphResultBinding :: Binding
    ,graphResultScore :: TypeScore
    } deriving Show

instance BinaryDefer GraphResult where
    put (GraphResult a b c d) = put3 a b c
    get = get3 (\a b c -> GraphResult a b c blankTypeScore)


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
    {aliases :: Aliases
    ,instances :: Instances
    ,costs :: IndexMutable Cost
    ,graph :: Map.Map TypePair (Lookup Node, Node)
    }


newGraph :: Aliases -> Instances -> [(Lookup Entry, ArgPos, TypeSig)] ->
            IndexMutable Cost -> (IndexMutable Cost, Graph)
newGraph as is xs cost = (costs sN, f (graph sN))
    where
        sN = execState (initialGraph xs >> populateGraph >> reverseLinks) s0
        s0 = S as is cost Map.empty

        f mp = Graph
            (fromListMany [(t,(c,v)) | (TypePair c t,(v,_)) <- Map.toList mp])
            (newIndex $ map snd $ sortFst $ Map.elems mp)


fromListMany :: Ord k => [(k,v)] -> Map.Map k [v]
fromListMany = Map.fromAscList . map (fst . head &&& map snd) . groupFst . sortFst


-- create the initial graph
initialGraph :: [(Lookup Entry, ArgPos, TypeSig)] -> State S ()
initialGraph xs = do
    is <- gets instances
    let f i xs@((t,_):_) = (t, (newLookup i, Node (sortOn graphResultEntry $ map snd xs) []))
        r = Map.fromAscList $ zipWith f [0..] $ groupFst $ sortFst $ map (newGraphResult is) xs
    modify $ \s -> s{graph=r}


-- create a result, and figure out what the relative is
newGraphResult :: Instances -> (Lookup Entry, ArgPos, TypeSig) -> (TypePair, GraphResult)
newGraphResult is (e,p,t) = (tp, GraphResult e p bind blankTypeScore)
    where (bind,tp) = alphaFlatten $ contextNorm is t


-- add links between each step
populateGraph :: State S ()
populateGraph = do
    todo <- liftM Map.keys $ gets graph
    f Set.empty todo
    where
        f done [] = return ()
        f done (t:odo) | Set.member t done = f done odo
        f done (t:odo) = do
            as <- gets aliases
            is <- gets instances
            let nxt = followNode as is t
            r <- mapM g nxt
            modify $ \s -> s{graph = Map.adjust (\(ni, Node a []) -> (ni, Node a r)) t $ graph s}
            f (Set.insert t done) (map fst3 nxt ++ odo)

        g (t,c,b) = do
            (costs,ci) <- liftM (getIndex c) $ gets costs
            modify $ \s -> s{costs=costs}

            mp <- gets graph
            ni <- case Map.lookup t mp of
                Just (ni,_) -> return ni
                Nothing -> do
                    let ni = Map.size mp
                    modify $ \s -> s{graph = Map.insert t (newLookup ni, Node [] []) mp}
                    return $ newLookup ni

            return (ni, ci, b)
            

-- follow a node to all possible next steps
-- all next ones must have already been alpha flattened
--
-- follow:
--  * Unboxing:     m a |-> a, M a |-> a
--  * Restriction:  M |-> a
--  * Alias:        a |-> alias(a)
--  * Context:      C a => a |-> a
--  * Membership:   C M => M |-> C a => a
followNode :: Aliases -> Instances -> TypePair -> [(TypePair, Cost, Binding)]
followNode as is (TypePair c t) = -- TODO: still need to alpha flatten
    [(TypePair c (f b), newCost $ CostUnbox a, fullBinding) | (TApp (TLit a) [b], f) <- contexts t]
    where
        fullBinding = map (id &&& id) [v | TVar v <- universe t]


-- add reverse links where you can, i.e. aliases
reverseLinks :: State S ()
reverseLinks = return () -- TODO


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

-- must search for each (node,bindings) pair

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
