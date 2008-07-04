
module Data.Binary.Defer.Map(
    module Data.Map
    ) where

import Control.Monad
import Data.Map
import Data.Binary.Defer
import qualified Data.Binary.Defer.Vector as Vector


instance (Ord k, BinaryDefer k, BinaryDefer v) => BinaryDefer (Map k v) where
    put = put . Vector.fromList . toAscList
    get = liftM (fromAscList . Vector.toList) get
