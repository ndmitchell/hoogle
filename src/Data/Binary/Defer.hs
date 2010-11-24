
module Data.Binary.Defer(
    module Data.Binary.Defer.Monad,
    module Data.Binary.Defer.Class,
    BinaryDeferGet(..)
    ) where

import Data.Binary.Defer.Monad
import Data.Binary.Defer.Class
import Data.Binary


class BinaryDeferGet a where
    binaryDeferGet :: DeferGet (Get a)
