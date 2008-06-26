
module Data.Binary.Defer.Vector(
    Vector, toList, fromList
    ) where

import Control.Monad
import Data.Binary.Defer


newtype Vector a = Vector [a]


instance BinaryDefer a => BinaryDefer (Vector a) where
    put (Vector xs) = putDefer $ do
        putInt (length xs)
        mapM_ put xs

    get = getDefer $ do
        i <- getInt
        liftM Vector $ replicateM i get


toList :: Vector a -> [a]
toList (Vector xs) = xs


fromList :: [a] -> Vector a
fromList = Vector
