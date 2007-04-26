
module Hoogle.DataBase.Modules(Modules, createModules, getModuleFromId) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Array
import Data.Binary.Defer

import Hoogle.Item.All
import Hoogle.DataBase.BinaryDefer


data Modules = Modules (Array ModId [String])
               deriving Show

instance BinaryDefer Modules where
    bothDefer = defer [\ ~(Modules a) -> unit Modules << a]


-- take a list of modules, which have a junk module id
-- return a module data structure, and the new list of modules
createModules :: [Item] -> ([Item],Modules)
createModules items =
        (map rename items, Modules $ listArray (0,length mods - 1) mods)
    where
        mods = Set.toAscList $ Set.fromList $ concatMap getModu items
        modmap = Map.fromAscList $ zip mods [0..]
        
        getModu = tail . inits . modName . itemMod
        rename item@Item{itemMod=Module _ modu} = item{itemMod=Module id2 modu}
            where id2 = fromJust $ Map.lookup modu modmap


getModuleFromId :: Modules -> ModId -> Module
getModuleFromId (Modules x) i = Module i (x ! i)

