
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Instances
import Hoogle.DataBase.TypeSearch.Aliases
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

instance BinaryDefer Graphs where
    put = undefined
    get = undefined

instance Show Graphs where
    show = undefined


---------------------------------------------------------------------
-- GRAPHS CONSTRUCTION

newGraphs :: Instances -> Aliases -> [(Lookup Entry, TypeSig)] -> Graphs
newGraphs is as xs = Graphs argGraph resGraph (indexFreeze cs3)
    where
        cs1 = newIndexMutable
        (cs2,argGraph) = newGraph is as (concat args) cs1
        (cs3,resGraph) = newGraph is as res cs2

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
    }
    

-- the pending information about an Entry, before it has been added
-- as a result
data Info = Info

type GraphsResult = (Lookup Entry,EntryView,TypeScore)


-- sorted by TypeScore
graphsSearch :: Graphs -> TypeSig -> [GraphsResult]
graphsSearch gs (TypeSig con ts) = evalState search s0
    where
        s0 = S IntMap.empty IntHeap.empty (resG:argsG) 0
        argsG = map (graphSearch (costs gs) (argGraph gs) . TypeSig con) args
        resG = graphSearch (costs gs) (resGraph gs) (TypeSig con res)

        (args,res) = initLast $ fromTFun ts


search :: State S [GraphsResult]
search = do
    searchFollow
    xs <- searchFound
    nxt <- searchNext
    ys <- searchFound
    zs <- if nxt then search else return []
    return $ xs++ys++zs


-- move results into the pile
searchFollow :: State S ()
searchFollow = undefined


-- return the results from the pile
searchFound :: State S [GraphsResult]
searchFound = do
    p <- gets pending
    c <- gets costMin
    (res,p) <- return $ IntHeap.popUntil c p
    modify $ \s -> s{pending=p}
    return res


-- return False if you can't move anywhere
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
                ,costMin=sum (map graphCost gs)}
            return True



newInfo :: Info
newInfo = undefined

-- add information to an info node
addInfo :: GraphResult -> Info -> (Info, [GraphsResult])
addInfo = undefined
