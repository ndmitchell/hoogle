
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
        f item@Item{itemRest=ItemInstance _} = return item
        
        f x = do
            i <- liftM fromInteger $ hTell hndl
            saveItem hndl x
            return $ x{itemId = Just i}


saveItem :: Handle -> Item -> IO ()
saveItem hndl item = do
    saveMod  (itemMod item)
    saveName (itemName item)
    saveTypeArgs (itemType item)
    saveRest (itemRest item)
    where
        saveMod Nothing = putInt 0
        saveMod (Just (ModuleId x)) = putInt x
        
        saveName Nothing = putString ""
        saveName (Just x) = putString x
        
        saveTypeArgs Nothing = putByte 0
        saveTypeArgs (Just (TypeAST (TypeSig con typ))) = do
            putByte 1
            putString (showConstraint con)
            let args = splitFun typ
            putByte (length args)
            mapM_ (putString . show) args

        saveTypeStr (TypeAST typ) = do
            putString (show typ)


        saveRest (ItemModule) = putByte 0
        saveRest (ItemClass lhs) = putByte 1 >> saveLHS lhs
        saveRest (ItemFunc) = putByte 2
        saveRest (ItemAlias lhs rhs) = putByte 3 >> saveLHS lhs >> saveTypeStr rhs
        saveRest (ItemData kw lhs) = putByte 4 >> putByte (fromEnum kw) >> saveLHS lhs
        saveRest (ItemKeyword) = putByte 5
        
        saveLHS (LHS con free) = do
            putString (showConstraint con)
            putString (concatMap (' ':) free)
        
        putString = hPutString hndl
        putByte = hPutByte hndl
        putInt = hPutInt hndl
        


loadItem :: Handle -> IO Item
loadItem hndl = return $ blankItem{itemName=Just "todo"}
