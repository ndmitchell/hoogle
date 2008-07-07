{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

-- TODO: Base this module on a suitable graphstate abstraction

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
import qualified Data.IntMap as IntMap
import qualified Data.IntHeap as IntHeap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State  hiding (get,put)
import qualified Control.Monad.State as S
import General.Code
import Data.Key


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
showGraph cs (Graph mp ns) = unlines $ concatMap (uncurry f) $ IntMap.elems mp2
    where
        mp2 :: IntMap.IntMap (TypePair,Node)
        mp2 = IntMap.fromList [(lookupKey n, (TypePair c t, lookupIndex n ns)) | (t,as) <- Map.toList mp, (c,n) <- as]

        f t (Node res link) = show t : map ("    " ++) (results : map g link)
            where
                results = if null rs then "No results" else unwords rs
                rs = [show a ++ "." ++ show b ++ (if null c then "" else show c) | GraphResult a b c _ <- res]

        g (ni,ci,b) = show (lookupIndex ci cs) ++ " ==> " ++
                      show (fst $ mp2 IntMap.! lookupKey ni) ++ " " ++ show b


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
    -- ensure "a" is in the graph, so you have a default
    -- start point
    f Set.empty (TypePair [] (TVar "a") : todo)
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
followNode as is (TypePair con t) =
        -- TODO: Should do something sensible with bindings
        [(snd $ alphaFlatten a, newCost b, c) | (a,b,c) <- next]
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
    {graphGS :: Map.Map Type [(TypeContext, Lookup Node)]
    ,nodesGS :: Index Node
    ,costsGS :: Index Cost
    ,found :: [GraphResult]
    ,reached :: Map.Map (Lookup Node, Binding) TypeScore
    ,available :: IntHeap.IntHeap (Lookup Node, Binding, Cost)
    }


-- TODO: Should be more lennient in where you start
-- ideally, strip context, follow aliases and unbox, and eventually just "a"
graphSearch :: Aliases -> Instances -> Index Cost -> Graph -> TypeSig -> GraphSearch
graphSearch as is costs g@(Graph a b) t = fromJust $ do
        i <- graphStart g t2
        return $ graphAdd i bind blankTypeScore s0
    where
        s0 = GraphSearch a b costs [] Map.empty IntHeap.empty
        (bind,t2) = alphaFlatten $ contextNorm is t


graphStart :: Graph -> TypePair -> Maybe (Lookup Node)
graphStart (Graph mp _) (TypePair c t) = do
    r <- Map.lookup t mp
    return $ snd $ head $ sortWith f r
    where
        -- ideally equal, otherwise one with fewer contexts
        f (c2,_) | null more = Left (length less)
                 | otherwise = Right (length more)
            where more = c2 \\ c    
                  less = c  \\ c2


-- you have reached a node
graphAdd :: Lookup Node -> Binding -> TypeScore -> GraphSearch -> GraphSearch
graphAdd ni bind score gs | (ni,bind) `Map.member` reached gs = gs
                          | otherwise =
    gs{reached = Map.insert (ni,bind) score $ reached gs
      ,found = res ++ found gs
      ,available = IntHeap.pushList nxt $ available gs}
    where
        Node gr link = lookupIndex ni (nodesGS gs)
        res = [x{graphResultScore = score} | x <- gr]
        nxt = [(costScore c, (ni,[],c)) | (ni,ci,bind) <- link, let c = lookupIndex ci (costsGS gs)]


-- those entires which are at a newly discovered node
graphFound :: GraphSearch -> [GraphResult]
graphFound = found


-- what is the minimum cost any future graphFound result may have
graphCost :: GraphSearch -> CostScore
graphCost = fromMaybe maxBound . IntHeap.min . available


-- follow a graph along a cost edge, that is now free
graphFollow :: Lookup Cost -> GraphSearch -> GraphSearch
graphFollow _ gs = gs{found=[]}


-- ask what possible node could be followed next
graphNext :: GraphSearch -> Maybe (Lookup Cost, Cost)
graphNext gs = Nothing
