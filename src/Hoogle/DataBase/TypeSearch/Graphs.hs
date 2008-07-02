
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Instances
import Hoogle.DataBase.TypeSearch.Aliases
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.TypeSearch.Cost
import Hoogle.DataBase.Item
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.TypeSig.All


data Graphs = Graphs
    {argGraph :: Graph -- the arguments
    ,resGraph :: Graph -- the results
    ,costs :: Costs -- how much each link costs
    }

instance BinaryDefer Graphs where
    put = undefined
    get = undefined

instance Show Graphs where
    show = undefined


graphs :: Instances -> Aliases -> [(Lookup Entry, TypeSig)] -> Graphs
graphs is as xs = Graphs argGraph resGraph undefined
    where
        cs1 = newIndexMutable
        (cs2,argGraph) = graph is as (concat args) cs1
        (cs3,resGraph) = graph is as res cs2

        (res,args) = unzip [((e, 0, TypeSig con res)
                            ,zipWith (\i t -> (e, i, TypeSig con t)) [0..] args)
            | (e, TypeSig con t) <- xs, let ts = fromTFun t, let (args,res) = (init ts, last ts)]




-- sorted by TypeScore
graphsSearch :: Graphs -> TypeSig -> [(Lookup Entry,EntryView,TypeScore)]
graphsSearch = undefined
