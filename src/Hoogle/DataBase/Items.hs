
module Hoogle.DataBase.Items where

import Hoogle.TypeSig.All
import Hoogle.TextBase.All

import System.IO
import Data.List
import General.Binary
import Control.Monad


-- populate the itemId field as you go
saveItems :: Handle -> [Item] -> IO [Item]
saveItems hndl tb = mapM f tb
    where
        f x = do
            i <- liftM fromInteger $ hTell hndl
            hPutStr hndl $ show x -- hacky for now :)
            return $ x{itemId = Just i}


loadItem :: Handle -> Int -> IO Item
loadItem hndl idn = return $ blankItem{itemName=Just "todo"}
