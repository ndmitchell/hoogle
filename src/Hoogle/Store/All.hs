
module Hoogle.Store.All(
    module Hoogle.Store.Monad,
    module Hoogle.Store.Class,
    BinaryDeferGet(..),
    FixedBinary(..)
    ) where

import Hoogle.Store.Monad
import Hoogle.Store.Class
import Data.Binary


class BinaryDeferGet a where
    binaryDeferGet :: DeferGet (Get a)

class FixedBinary a where
    fixedSize :: a -> Int
