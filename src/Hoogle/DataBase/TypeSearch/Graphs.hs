
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.Result
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.TypeSearch.TypeScore
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
newGraphs as is xs = Graphs (newIndex entries) argGraph resGraph
    where
        entries = [EntryInfo e (length (fromTFun t2) - 1) c2
                  |(e,t) <- xs, let TypeSimp c2 t2 = normContext is t]

        argGraph = newGraph as (concat args)
        resGraph = newGraph as res

        (args,res) = unzip
            [ initLast $ zipWith (\i t -> (lnk, i, t)) [0..] $ fromTFun t
            | (i, e, (_, TypeSig _ t)) <- zip3 [0..] entries xs, let lnk = newLink i e]


---------------------------------------------------------------------
-- GRAPHS SEARCHING


-- sorted by TypeScore
graphsSearch :: Aliases -> Instances -> Graphs -> TypeSig -> [Result]
graphsSearch as is gs t = resultsCombine is con (length args) ans
    where
        ans = mergesBy (compare `on` resultArgScore . snd) $ 
              f Nothing (resGraph gs) res :
              zipWith (\i -> f (Just i) (argGraph gs)) [0..] args

        f a g = map ((,) a) . graphSearch as g
        (args,res) = initLast $ fromTFun ts
        TypeSimp con ts = normContext is t


data S = S
    {infos :: IntMap.IntMap (Maybe ResultAll) -- Int = Lookup EntryInfo
    ,pending :: Heap.Heap TypeScore Result
    ,todo :: [(Maybe ArgPos, ResultArg)]
    ,arity :: Int
    ,instances :: Instances
    ,qcontext :: TypeContext
    }


resultsCombine :: Instances -> TypeContext -> Int -> [(Maybe ArgPos, ResultArg)] -> [Result]
resultsCombine is context arity xs = evalState delResult s0
    where s0 = S IntMap.empty Heap.empty xs arity is context


-- Heap -> answer
delResult :: State S [Result]
delResult = do
    pending <- gets pending
    todo <- gets todo
    case todo of
        [] -> concatMapM (f . snd) $ Heap.toList pending
        t:odo -> do
            modify $ \s -> s{todo = odo}
            let (res,hp) = Heap.popUntil (resultArgScore $ snd t) pending
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
    let entryId = linkKey $ resultArgEntry val
    infs <- gets infos
    arity <- gets arity
    is <- gets instances
    qcontext <- gets qcontext
    let def = newResultAll arity (fromLink $ resultArgEntry val)
    case IntMap.lookup entryId infs of
        Just Nothing -> return ()
        Nothing | isNothing def -> modify $ \s -> s{infos = IntMap.insert entryId Nothing (infos s)}
        x -> do
            let inf = fromJust $ fromMaybe def x
            (inf,res) <- return $ addResultAll is qcontext (arg,val) inf
            res <- return $ map (thd3 &&& id) res
            modify $ \s -> s
                {infos = IntMap.insert entryId (Just inf) (infos s)
                ,pending = Heap.pushList res (pending s)
                }
