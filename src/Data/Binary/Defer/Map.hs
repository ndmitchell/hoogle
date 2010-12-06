{-|
    Provides a BinaryDefer instance for Data.Map

    Defers the entire thing, and every element, but not any keys
    which should give just about the right balance of laziness
-}
module Data.Binary.Defer.Map(module Map) where

import Control.Monad
import Control.Arrow
import Data.Map as Map -- FIXME: If this is qualified, then the export doesn't work - no idea why
import Data.Binary.Defer


instance (Ord k, BinaryDefer k, BinaryDefer v) => BinaryDefer (Map.Map k v) where
    put = putDefer . put . Vector . Prelude.map (second Defer) . Map.toAscList
    get = getDefer $ fmap (Map.fromAscList . Prelude.map (second fromDefer) . fromVector) get

newtype Vector a = Vector {fromVector :: [a]}

instance BinaryDefer a => BinaryDefer (Vector a) where
    put (Vector xs) = putDefer $ do
        putInt (length xs)
        mapM_ put xs

    get = getDefer $ do
        i <- getInt
        liftM Vector $ replicateM i get
