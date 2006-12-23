
module Hoogle.DataBase.Items where

import Hoogle.TypeSig.All
import Hoogle.TextBase.All
import Hoogle.DataBase.Docs

import System.IO
import Data.List
import Data.Maybe
import General.Binary
import Control.Monad


-- populate the itemId field as you go
saveItems :: Handle -> [Item ()] -> Maybe String -> IO [Item ()]
saveItems hndl tb haddock = f Nothing tb
    where
        f :: Maybe ([String],Haddock) -> [Item ()] -> IO [Item ()]
        f had [] = return []
        f had (x:xs) | isItemInstance $ itemRest x = f had xs >>= return . (x:)
        
        f had (x:xs) = do
            i <- hGetPos hndl

            let modname = modName $ fromJust $ itemMod x
            
            had2 <- if isItemModule (itemRest x) || isNothing haddock then return Nothing
                    else case had of
                         Just (n,h) | n == modname -> return $ Just h
                         _ -> loadHaddock (fromJust haddock) modname

            saveItem had2 hndl x
            
            let x2 = x{itemId = Just i}
            xs2 <- f (had2 >>= return . (,) modname) xs
            return $ x2:xs2


saveItem :: Maybe Haddock -> Handle -> Item () -> IO ()
saveItem haddock hndl item = do
        start <- hGetPos hndl
        putInt 0
        saveMod  (itemMod item)
        saveName (itemName item)
        saveTypeArgs (itemType item)
        saveRest (itemRest item)
        saveDocs start haddock
    where
        saveDocs start Nothing = return ()
        saveDocs start (Just haddock) = do
            docs <- hGetPos hndl
            b <- saveDocsHandle hndl haddock item
            when b $ do
                now <- hGetPos hndl
                hSetPos hndl start
                putInt docs
                hSetPos hndl now

        saveMod Nothing = putInt 0
        saveMod (Just modu) = putInt $ modId modu
        
        saveName Nothing = putString ""
        saveName (Just x) = putString x
        
        saveTypeArgs Nothing = putByte 0
        saveTypeArgs (Just (TypeAST (TypeSig con typ))) = do
            putByte 1
            putString (showConstraint con)
            let args = splitFun typ
            putByte (length args)
            mapM_ (putString . showFun) args

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
        


loadItem :: Handle -> IO (Item ())
loadItem hndl = do
        _ <- getInt -- documentation
        a <- loadMod
        b <- loadName
        c <- loadTypeArgs
        d <- loadRest
        return $ Item a b c Nothing () d
    where
        loadMod = do
            x <- getInt
            return $ if x == 0 then Nothing else Just (Module [] x)
        
        loadName = do
            x <- getString
            return $ if null x then Nothing else Just x
        
        loadTypeArgs = do
            x <- getByte
            case x of
                0 -> return Nothing
                1 -> do
                    con <- getString
                    n <- getByte
                    args <- replicateM n getString
                    return $ Just $ TypeArgs con args
        
        loadTypeStr = liftM TypeStr getString
        
        loadRest = do
            x <- getByte
            case x of
                0 -> return ItemModule
                1 -> liftM ItemClass $ loadLHS
                2 -> return ItemFunc
                3 -> do {a <- loadLHS; b <- loadTypeStr; return $ ItemAlias a b}
                4 -> do {a <- getByte; b <- loadLHS; return $ ItemData (toEnum a) b}
                5 -> return ItemKeyword

        loadLHS = do
            a <- getString
            b <- getString
            return $ LHSStr a b
        
        getString = hGetString hndl
        getByte = hGetByte hndl
        getInt = hGetInt hndl
