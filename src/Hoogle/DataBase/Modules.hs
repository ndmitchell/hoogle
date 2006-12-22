
module Hoogle.DataBase.Modules(Modules, saveModules, loadModules, getModuleFromId) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import General.Binary
import Data.List
import Data.Array
import Data.Maybe
import Control.Monad

import Hoogle.TextBase.All


data Modules = Modules (Array Int [String])



-- each module should be listed exactly once
-- module Data.Map, is given as in module Data, named Map

-- 0 is now allowed as a module Id
saveModules :: Handle -> TextBase -> IO [Item ()]
saveModules hndl tb = do
        hPutInt hndl $ length modus
        mapM_ f modus
        let mp = Map.fromAscList $ zip modus [1..]
        return $ map (g mp) res
    where
        res = populateModules tb
        modus = map head $ group $ sort $ map (fromModule . fromJust . itemMod) res
        fromModule (Module x) = x

        f modu = do
            hPutInt hndl $ length modu
            mapM_ (hPutString hndl) modu

        g mp x@Item{itemMod=Just (Module modu)} = x{itemMod = Just (ModuleId (Map.findWithDefault 0 modu mp))}

        populateModules :: [Item ()] -> [Item ()]
        populateModules xs = f [] xs
            where
                f modu (item@Item{itemMod=Just (Module xs),itemName=Just x,itemRest=ItemModule} : rest)
                        = item : f (xs ++ [x]) rest
                f modu (x:xs) = x{itemMod=Just (Module modu)} : f modu xs
                f modu [] = []


loadModules :: Handle -> IO Modules
loadModules hndl = do
        count <- hGetInt hndl
        items <- replicateM count f
        return $ Modules $ listArray (1,count) items
    where
        f = do
            i <- hGetInt hndl
            replicateM i (hGetString hndl)


getModuleFromId :: Modules -> Int -> [String]
getModuleFromId (Modules x) i = x ! i

