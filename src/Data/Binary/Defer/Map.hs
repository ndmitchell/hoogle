{-|
    Provides a BinaryDefer instance for Data.Map

    Defers the entire thing, and every element, but not any keys
    which should give just about the right balance of laziness
-}

module Data.Binary.Defer.Map(
    module Data.Map
    ) where

import Control.Monad
import Control.Arrow
import Data.Map
import Data.Binary.Defer
import Data.Binary.Defer.Vector as Vector


instance (Ord k, BinaryDefer k, BinaryDefer v) => BinaryDefer (Data.Map.Map k v) where
    put = putDefer . put . Vector.fromList . Prelude.map (id *** Defer) . Data.Map.toAscList
    get = getDefer $ liftM (Data.Map.fromAscList . Prelude.map (id *** fromDefer) . Vector.toList) get
