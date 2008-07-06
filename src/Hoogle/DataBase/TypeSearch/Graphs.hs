
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.Item
import Hoogle.TypeSig.All

import Data.Binary.Defer
import Data.Binary.Defer.Index
import qualified Data.IntMap as IntMap
import qualified Data.IntHeap as IntHeap
import General.Code
import Control.Monad.State


-- for resGraph, the associated ArgPos is the arity of the function

data Graphs = Graphs
    {argGraph :: Graph -- the arguments
    ,resGraph :: Graph -- the results
    ,costs :: Index Cost -- how much each link costs
    }

instance Show Graphs where
    show (Graphs a b c) = "== Arguments ==\n\n" ++ showGraph c a ++
                          "\n== Results ==\n\n" ++ showGraph c b

instance BinaryDefer Graphs where
    put (Graphs a b c) = put3 a b c
    get = get3 Graphs


---------------------------------------------------------------------
-- GRAPHS CONSTRUCTION

newGraphs :: Aliases -> Instances -> [(Lookup Entry, TypeSig)] -> Graphs
newGraphs as is xs = Graphs argGraph resGraph (indexFreeze cs3)
    where
        cs1 = newIndexMutable
        (cs2,argGraph) = newGraph as is (concat args) cs1
        (cs3,resGraph) = newGraph as is res cs2

        (args,res) = unzip
            [ initLast $ zipWith (\i t -> (e, i, TypeSig con t)) [0..] $ fromTFun t
            | (e, TypeSig con t) <- xs]




---------------------------------------------------------------------
-- GRAPHS SEARCHING

{-
Data Structures:

1) Map EntryId (Maybe Info)

Nothing means either the EntryId has been given back, or the
arity of the EntryId is too high to work. Accumulate results
and arguments in the Info structure. Once you have a complete
set of information on an entry, including any Alpha/ArgPos negative
scores, move it to a second pile.

2) Pending

Pending has the minimum score any item could have, and a list of items
which have been completed and are awaiting outputting. Once an item
gets below the minimum score it is outputted.

3) [(Graph,Score)]

A list of graphs, each with their minimum possible score. The sum of
these scores is the minimum used by Pile.
-}


data S = S
    {infos :: IntMap.IntMap (Maybe Info) -- Int = Lookup Entry
    ,pending :: IntHeap.IntHeap GraphsResult -- Int = CostScore
    ,graphs :: [GraphSearch] -- first graph is the result graph
    ,costMin :: CostScore -- the lowest cost you may return next
    ,numArgs :: Int
    }
    

type GraphsResult = (Lookup Entry,[EntryView],TypeScore)


-- sorted by TypeScore
graphsSearch :: Aliases -> Instances -> Graphs -> TypeSig -> [GraphsResult]
graphsSearch as is gs (TypeSig con ts) = evalState search s0
    where
        s0 = S IntMap.empty IntHeap.empty (resG:argsG) 0 (length argsG)
        argsG = map (graphSearch as is (costs gs) (argGraph gs) . TypeSig con) args
        resG = graphSearch as is (costs gs) (resGraph gs) (TypeSig con res)

        (args,res) = initLast $ fromTFun ts


search :: State S [GraphsResult]
search = do
    searchFollow
    xs <- searchFound
    nxt <- searchNext
    ys <- searchFound
    zs <- if nxt then search else return []
    return $ xs++ys++zs


-- move results from graphFound into the pile
searchFollow :: State S ()
searchFollow = do
    gs <- gets graphs
    mapM_ f [(i,g2) | (i,g) <- zip (Nothing : map Just [0..]) gs, g2 <- graphFound g]
    where
        f (arg,val) = do
            let entryId = lookupKey $ graphResultEntry val
            infs <- gets infos
            numArgs <- gets numArgs
            case IntMap.findWithDefault (Just $ newInfo numArgs) entryId infs of
                Nothing -> return ()
                Just inf -> do
                    (inf,res) <- return $ addInfo arg val inf
                    res <- return $ map (typeScoreTotal . thd3 &&& id) res
                    modify $ \s -> s
                        {infos = IntMap.insert entryId inf (infos s)
                        ,pending = IntHeap.pushList res (pending s)
                        }


-- return the results from the pile satisfying costMin
searchFound :: State S [GraphsResult]
searchFound = do
    p <- gets pending
    c <- gets costMin
    (res,p) <- return $ IntHeap.popUntil c p
    modify $ \s -> s
        {pending=p
        ,infos=foldr (uncurry IntMap.insert) (infos s)
                     [(lookupKey $ fst3 r, Nothing) | r <- res]
        }
    return res


-- return False if you can't move anywhere, update costMin
searchNext :: State S Bool
searchNext = do
    gs <- gets graphs
    case mapMaybe graphNext gs of
        [] -> do
            modify $ \s -> s{costMin=maxBound}
            return False
        xs -> do
            let i = fst $ minimumBy (compare `on` snd) xs
            gs <- return $ map (graphFollow i) gs
            modify $ \s -> s
                {graphs=gs
                ,costMin=minimum (map graphCost gs)}
            return True


-- the pending information about an Entry, before it has been added
-- as a result
type Info = [[GraphResult]]


newInfo :: Int -> Info
newInfo arity = replicate (arity+1) []

-- add information to an info node
addInfo :: Maybe ArgPos -> GraphResult -> Info -> (Maybe Info, [GraphsResult])
addInfo Nothing res ([]:is) | arityEntry < arityQuery || arityEntry > arityQuery + 2 = (Nothing,[])
    where
        arityQuery = length is
        arityEntry = graphResultPos res

addInfo pos res info = (Just info2, if any null info2 then [] else ans)
    where
        ind = maybe 0 (+1) pos
        info2 = zipWith (\i x -> [res|i==ind] ++ x) [0..] info

        arityEntry = graphResultPos $ head $ head info
        badargs = replicate (1 + arityEntry - length info) $ newCost CostDelArg
        ans = [ newGraphsResults badargs rs r
              | (r:rs) <- sequence $ info2 !!+ (ind,[res])
              , disjoint $ map graphResultPos rs]


-- given the results for each argument, and the result
-- create a final result structure
newGraphsResults :: [Cost] -> [GraphResult] -> GraphResult -> GraphsResult
newGraphsResults costs args res =
    (graphResultEntry res
    ,zipWith ArgPosNum [0..] $ map graphResultPos args
    ,newTypeScore $ costs ++ nub (concatMap (typeScoreCosts . graphResultScore) $ args++[res])
    )
