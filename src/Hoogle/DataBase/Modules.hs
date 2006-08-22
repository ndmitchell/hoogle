
module Hoogle.DataBase.Modules(Modules, saveModules) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import General.Binary
import Data.List
import Control.Monad

import Hoogle.TextBase.All


data Modules = Modules (Map.Map [String] Int)



-- each module should be listed exactly once
-- module Data.Map, is given as in module Data, named Map

saveModules :: Handle -> TextBase -> IO [(Int, Item)]
saveModules hndl tb = do
        hPutInt hndl $ length modus
        ids <- mapM f modus
        let mp = Map.fromAscList $ zip modus ids
        return $ map (g mp) res
    where
        res = populateModules tb
        modus = map head $ group $ sort $ map fst res

        f modu = do
            i <- liftM fromInteger $ hTell hndl
            hPutInt hndl $ length modu
            mapM_ (hPutStr hndl) modu
            return i

        g mp (a,b) = (Map.findWithDefault 0 a mp, b)

        populateModules :: [Item] -> [([String], Item)]
        populateModules xs = f [] xs
            where
                f modu (Module x:xs) = (init x, Module [last x]) : f x xs
                f modu (Instance{}:xs) = f modu xs
                f modu (x:xs) = (modu, x) : f modu xs
                f modu [] = []
