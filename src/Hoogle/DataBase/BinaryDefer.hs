-- instances for BinaryDefer for common types
-- this should definately be moved back into the binarydefer library!

module Hoogle.DataBase.BinaryDefer where

import Data.Binary.Defer
import Data.Binary.Defer.List
import Hoogle.Item.All

import Data.Array
import Control.Monad


concatMapM f = liftM concat . mapM f


instance (Ix a, BinaryDefer a, BinaryDefer b) => BinaryDefer (Array a b) where
    putDefer hndl xs = do
        a <- putDefer hndl (bounds xs)
        b <- mapM (putDefer hndl) (elems xs)
        return $ a ++ concat b

    get hndl = do 
        bound <- get hndl
        xs <- replicateM (rangeSize bound) (get hndl)
        return $ listArray bound xs

instance BinaryDefer Item where
    bothDefer = error "BinaryDefer Item, todo"

instance (BinaryDefer a, BinaryDefer b) => BinaryDefer (a,b) where
    bothDefer = defer [\ ~(a,b) -> unit (,) << a << b]
