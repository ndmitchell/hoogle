
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.Result
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Aliases
import Hoogle.DataBase.TypeSearch.TypeScore
import Hoogle.Type.All hiding (Result)

import Hoogle.Store.All
import qualified Data.IntMap as IntMap
import qualified General.Heap as Heap
import General.Base
import General.Util
import Control.Monad.Trans.State


-- for resGraph, the associated ArgPos is the arity of the function

data Graphs = Graphs
    {argGraph :: Graph -- the arguments
    ,resGraph :: Graph -- the results
    }

instance NFData Graphs where
    rnf (Graphs a b) = rnf (a,b)

instance Show Graphs where
    show (Graphs a b) = "== Arguments ==\n\n" ++ show a ++
                        "\n== Results ==\n\n" ++ show b

instance Store Graphs where
    put (Graphs a b) = put2 a b
    get = get2 Graphs


---------------------------------------------------------------------
-- GRAPHS CONSTRUCTION

newGraphs :: Aliases -> Instances -> [(TypeSig, Once Entry)] -> Graphs
newGraphs as is xs = Graphs argGraph resGraph
    where
        entries = [ (t2, e2{entryInfoKey=i, entryInfoEntries=map snd ys})
                  | (i, ys@(((t2,e2),_):_)) <- zip [0..] $ sortGroupFst $ map (\(t,e) -> (normType as is t, e)) xs]

        argGraph = newGraph (concat args)
        resGraph = newGraph res

        (args,res) = unzip
            [ initLast $ zipWith (\i t -> (lnk, i, t)) [0..] $ fromTFun t
            | (t, e) <- entries, let lnk = once e]


normType :: Aliases -> Instances -> TypeSig -> (Type, EntryInfo)
normType as is t = (t3, EntryInfo 0 [] (length (fromTFun t3) - 1) c2 a)
    where TypeSimp c2 t2 = normInstances is t
          (a,t3) = normAliases as t2

---------------------------------------------------------------------
-- GRAPHS SEARCHING


-- sorted by TypeScore
graphsSearch :: Aliases -> Instances -> Graphs -> TypeSig -> [ResultReal]
graphsSearch as is gs t = resultsCombine is query ans
    where
        ans = mergesBy (comparing $ resultArgBind . snd) $ 
              f Nothing (resGraph gs) res :
              zipWith (\i -> f (Just i) (argGraph gs)) [0..] args

        f a g = map ((,) a) . graphSearch g
        (args,res) = initLast $ fromTFun ts
        (ts,query) = normType as is t


data S = S
    {infos :: IntMap.IntMap (Maybe ResultAll) -- Int = Once EntryInfo
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
                (\_ _ -> Just Nothing) (entryInfoKey $ fromOnce $ fst3 r) infos
            if isNothing res then return [] else do
                modify $ \s -> s{infos=infos}
                return [r]


-- todo -> heap/info
addResult :: Maybe ArgPos -> ResultArg -> State S ()
addResult arg val = do
    let entId = entryInfoKey $ fromOnce $ resultArgEntry val
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
