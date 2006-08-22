
module Hoogle.DataBase.Type(DataBase, createDataBase) where

import Data.IORef
import System.IO
import Control.Monad

import Hoogle.DataBase.Alias
import Hoogle.DataBase.Kinds
import Hoogle.DataBase.Instances
import Hoogle.DataBase.Items
import Hoogle.DataBase.Modules
import Hoogle.DataBase.Texts
import Hoogle.TextBase.All

import General.Binary


hooVersion = 1 :: Int


type Pending x = IORef (Either Int x)


data DataBase = DataBase {
                    handle  :: Handle,
                    package :: String,
                    version :: String,
                    
                    -- the static and cached information
                    modules :: Pending Modules, -- Prelude, Data.Map etc.
                    kinds :: Pending Kinds, -- [] 1, Ord 1
                    alias :: Pending Alias, -- type String = [Char]
                    instances :: Pending Instances, -- instance Ord Bool
                    
                    -- the dynamic information
                    nameSearchPos, typeSearchPos :: Int
                }


-- [] is success
-- (_:_) are the error messages
createDataBase :: TextBase -> FilePath -> IO [String]
createDataBase tb file = do
    hndl <- openBinaryFile file WriteMode
    hPutStr hndl "HOOG"
    hPutInt hndl 0 -- 0 for binary notice
    hPutInt hndl hooVersion -- verson number
    tablePos <- hGetPosn hndl
    replicateM_ 8 $ hPutInt hndl 0
    
    posModule <- hTellInt hndl
    tb2 <- saveModules hndl tb
    tb3 <- saveItems hndl tb2
    
    (pos, err) <-
        mapAndUnzipM (\x -> do y <- hTellInt hndl ; z <- x ; return (y,z))
            [hPutString hndl "package" >> return []
            ,hPutString hndl "1.0" >> return []
            ,saveKinds hndl tb
            ,saveAlias hndl tb
            ,saveInstances hndl tb
            ,saveTexts hndl tb3
            ,return [] -- save types
            ]
    
    hSetPosn tablePos
    mapM_ (hPutInt hndl) (posModule:pos)
    hClose hndl
    
    return $ concat err

