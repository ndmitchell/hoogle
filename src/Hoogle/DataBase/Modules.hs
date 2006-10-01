
module Hoogle.DataBase.Modules(Modules, saveModules) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import General.Binary
import Data.List
import Data.Maybe
import Control.Monad

import Hoogle.TextBase.All


data Modules = Modules (Map.Map [String] Int)



-- each module should be listed exactly once
-- module Data.Map, is given as in module Data, named Map

saveModules :: Handle -> TextBase -> IO [Item]
saveModules hndl tb = do
        hPutInt hndl $ length modus
        ids <- mapM f modus
        let mp = Map.fromAscList $ zip modus ids
        return $ map (g mp) res
    where
        res = populateModules tb
        modus = map head $ group $ sort $ map (fromModule . fromJust . itemMod) res
        fromModule (Module x) = x

        f modu = do
            i <- liftM fromInteger $ hTell hndl
            hPutInt hndl $ length modu
            mapM_ (hPutStr hndl) modu
            return i

        g mp x@Item{itemMod=Just (Module modu)} = x{itemMod = Just (ModuleId (Map.findWithDefault 0 modu mp))}

        populateModules :: [Item] -> [Item]
        populateModules xs = f [] xs
            where
                f modu (item@Item{itemMod=Just (Module xs),itemName=Just x,itemRest=ItemModule} : rest)
                        = item : f (xs ++ [x]) rest
                f modu (x:xs) = x{itemMod=Just (Module modu)} : f modu xs
                f modu [] = []
