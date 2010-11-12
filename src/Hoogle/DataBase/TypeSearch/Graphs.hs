
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.Result
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.TypeSearch.TypeScore
import Hoogle.Type.All

import Data.Binary.Defer
import Data.Binary.Defer.Index
import qualified Data.IntMap as IntMap
import qualified Data.Heap as Heap
import General.Code
import Control.Monad.State


-- for resGraph, the associated ArgPos is the arity of the function

data Graphs = Graphs
    {entryInfo :: Index EntryInfo
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
newGraphs as is xs = Graphs (newIndex $ map snd entries) argGraph resGraph
    where
        entries = [ (t2, e2{entryInfoEntries = sortOn linkKey $ map snd ys})
                  | ys@(((t2,e2),_):_) <- sortGroupFst $ map (\(e,t) -> (normType as is t, e)) xs]

        argGraph = newGraph (concat args)
        resGraph = newGraph res

        (args,res) = unzip
            [ initLast $ zipWith (\i t -> (lnk, i, t)) [0..] $ fromTFun t
            | (i, (t, e)) <- zip [0..] entries, let lnk = newLink i e]


normType :: Aliases -> Instances -> TypeSig -> (Type, EntryInfo)
normType as is t = (t3, EntryInfo [] (length (fromTFun t3) - 1) c2 a)
    where TypeSimp c2 t2 = normInstances is t
          (a,t3) = normAliases as t2

---------------------------------------------------------------------
-- GRAPHS SEARCHING


-- sorted by TypeScore
graphsSearch :: Aliases -> Instances -> Graphs -> TypeSig -> [ResultReal]
graphsSearch as is gs t = resultsCombine is query ans
    where
        ans = mergesBy (compare `on` resultArgBind . snd) $ 
              f Nothing (resGraph gs) res :
              zipWith (\i -> f (Just i) (argGraph gs)) [0..] args

        f a g = map ((,) a) . graphSearch g
        (args,res) = initLast $ fromTFun ts
        (ts,query) = normType as is t


data S = S
    {infos :: IntMap.IntMap (Maybe ResultAll) -- Int = Link EntryInfo
    ,pending :: Heap.Heap Int Result
    ,todo :: [(Maybe ArgPos, ResultArg)]
    ,instances :: Instances
    ,query :: EntryInfo
    }


resultsCombine :: Instances -> EntryInfo -> [(Maybe ArgPos, ResultArg)] -> [ResultReal]
resultsCombine is query xs = flattenResults $ evalState delResult s0
    where s0 = S IntMap.empty Heap.empty xs is query


-- Heap -> answer
delResult :: State S [Result]
delResult = do
    pending <- gets pending
    todo <- gets todo
    case todo of
        [] -> concatMapM f $ Heap.elems pending
        t:odo -> do
            let (res,hp) = Heap.popWhile (costBinding $ resultArgBind $ snd t) pending
            modify $ \s -> s{todo=odo, pending=hp}
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
addResult :: Maybe ArgPos -> ResultArg -> State S ()
addResult arg val = do
    let entId = linkKey $ resultArgEntry val
    infs <- gets infos
    is <- gets instances
    query <- gets query
    let def = newResultAll query (resultArgEntry val)
    case IntMap.lookup entId infs of
        Just Nothing -> return ()
        Nothing | isNothing def -> modify $ \s -> s{infos = IntMap.insert entId Nothing $ infos s}
        x -> do
            let inf = fromJust $ fromMaybe def x
            (inf,res) <- return $ addResultAll is query (arg,val) inf
            res <- return $ map (costTypeScore . thd3 &&& id) res
            modify $ \s -> s
                {infos = IntMap.insert entId (Just inf) $ infos s
                ,pending = Heap.insertList res (pending s)
                }
