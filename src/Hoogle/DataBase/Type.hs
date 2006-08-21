
module Hoogle.DataBase.Type(DataBase, createDataBase) where

import Data.IORef
import System.IO
import Control.Monad

import Hoogle.DataBase.Alias
import Hoogle.DataBase.Kinds
import Hoogle.DataBase.Instances
import Hoogle.TextBase.All

import General.Binary



type Pending x = IORef (Either Int x)


data DataBase = DataBase {
                    handle  :: Handle,
                    package :: String,
                    version :: String,
                    
                    -- the static and cached information
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
    hPutStr hndl "HOOG\0\0\0\0"
    hPutInt hndl 1
    tablePos <- hGetPosn hndl
    replicateM_ 7 $ hPutInt hndl 0
    
    poserrs <-
        mapM (\x -> do y <- hTell hndl ; z <- x ; return (y,z))
            [hPutString hndl "package" >> return []
            ,hPutString hndl "1.0" >> return []
            ,saveKinds hndl tb
            ,saveAlias hndl tb
            ,saveInstances hndl tb
            ,return [] -- save text
            ,return [] -- save types
            ]
    let (pos,err) = (map fst poserrs, concatMap snd poserrs)
    
    hSetPosn tablePos
    mapM_ (hPutInt hndl . fromInteger) pos
    hClose hndl
    
    return err

