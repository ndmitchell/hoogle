
module Hoogle.DataBase.TypeSearch.Graphs where

import Hoogle.DataBase.TypeSearch.Graph
import Hoogle.DataBase.TypeSearch.Instances
import Hoogle.DataBase.TypeSearch.Aliases
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.Item
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.TypeSig.All


data Graphs = Graphs deriving Show

instance BinaryDefer Graphs where
    put = undefined
    get = undefined


data Answer = Answer
    Entry -- the entry Id
    EntryView -- the positions of arguments
    TypeScore -- the costs
    [Binding] -- the alpha renaming set



graphs :: Instances -> Aliases -> [(Lookup Entry, TypeSig)] -> Graphs
graphs = undefined


-- sorted by TypeScore
graphsSearch :: Graphs -> TypeSig -> [(Lookup Entry,EntryView,TypeScore)]
graphsSearch = undefined
