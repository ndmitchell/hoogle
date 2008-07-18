
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.Item.All
import Hoogle.TypeSig.All

import Data.Binary.Defer
import Data.Binary.Defer.Index
import qualified Data.IntMap as IntMap
import qualified Data.Heap as Heap
import General.Code
import Control.Monad.State


-- for resGraph, the associated ArgPos is the arity of the function

data Graphs = Graphs
    {costs :: Index Cost -- how much each link costs
    ,argGraph :: Graph -- the arguments
    ,resGraph :: Graph -- the results
    }

instance Show Graphs where
    show (Graphs a b c) = "== Arguments ==\n\n" ++ show b ++
                          "\n== Results ==\n\n" ++ show c

instance BinaryDefer Graphs where
    put (Graphs a b c) = put3 a b c
    get = do
        res@(Graphs a b c) <- get3 Graphs
        getDeferPut a
        return res


---------------------------------------------------------------------
-- GRAPHS CONSTRUCTION

newGraphs :: Aliases -> Instances -> [(Link Entry, TypeSig)] -> Graphs
newGraphs as is xs = Graphs (indexFreeze cs3) argGraph resGraph
    where
        cs1 = newIndex_
        (cs2,argGraph) = newGraph as is (concat args) cs1
        (cs3,resGraph) = newGraph as is res cs2

        (args,res) = unzip
            [ initLast $ zipWith (\i t -> (e, i, TypeSig con t)) [0..] $ fromTFun t
            | (e, TypeSig con t) <- xs]




---------------------------------------------------------------------
-- GRAPHS SEARCHING


type GraphsResult = (Link Entry,[EntryView],TypeScore)


-- sorted by TypeScore
graphsSearch :: Aliases -> Instances -> Graphs -> TypeSig -> [GraphsResult]
graphsSearch as is gs (TypeSig con ts) = resultsCombine (length args) ans
    where
        ans = mergesBy (compare `on` graphResultScore . snd) $ 
              f Nothing (resGraph gs) res :
              zipWith (\i -> f (Just i) (argGraph gs)) [0..] args

        f a g = map ((,) a) . graphSearch as is g . TypeSig con
        (args,res) = initLast $ fromTFun ts


data S = S
    {infos :: IntMap.IntMap (Maybe Info) -- Int = Lookup Entry
    ,pending :: Heap.Heap TypeScore GraphsResult
    ,todo :: [(Maybe ArgPos, GraphResult)]
    ,arity :: Int
    }


resultsCombine :: Int -> [(Maybe ArgPos, GraphResult)] -> [GraphsResult]
resultsCombine arity xs = evalState delResult s0
    where s0 = S IntMap.empty Heap.empty xs arity


-- Heap -> answer
delResult :: State S [GraphsResult]
delResult = do
    pending <- gets pending
    todo <- gets todo
    case todo of
        [] -> concatMapM (f . snd) $ Heap.toList pending
        t:odo -> do
            modify $ \s -> s{todo = odo}
            let (res,hp) = Heap.popUntil (graphResultScore $ snd t) pending
            ans1 <- concatMapM f res
            uncurry addResult t
            ans2 <- delResult
            return $ ans1 ++ ans2
    where
        f r = do
            infos <- gets infos
            (Just res,infos) <- return $ IntMap.updateLookupWithKey
                (\_ _ -> Just Nothing) (linkKey $ fst3 r) infos
            if isNothing res then return [] else do
                modify $ \s -> s{infos=infos}
                return [r]


-- todo -> heap/info
addResult :: Maybe ArgPos -> GraphResult -> State S ()
addResult arg val = do
    let entryId = linkKey $ graphResultEntry val
    infs <- gets infos
    arity <- gets arity
    case IntMap.findWithDefault (Just $ newInfo arity) entryId infs of
        Nothing -> return ()
        Just inf -> do
            (inf,res) <- return $ addInfo arg val inf
            res <- return $ map (thd3 &&& id) res
            modify $ \s -> s
                {infos = IntMap.insert entryId inf (infos s)
                ,pending = Heap.pushList res (pending s)
                }



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

        arityEntry = graphResultPos $ head $ head info2
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
    ,addTypeScores (vars++costs) $ mergeTypeScores $ map graphResultScore $ args++[res]
    )
    where vars = bindCost $ bindMerge $ map graphResultBinding (res:args)
