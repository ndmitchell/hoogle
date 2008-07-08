
module Data.TypeMap(
    TypeMap, empty,
    lookup, insert
    ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as Map

newtype TypeMap = TypeMap (Map.Map TypeRep Dynamic)


empty :: TypeMap
empty = TypeMap Map.empty


lookup :: Typeable a => TypeMap -> Maybe a
lookup (TypeMap mp) = res
    where res = liftM (fromJust . fromDynamic) $ Map.lookup (typeOf $ fromJust res) mp


insert :: Typeable a => a -> TypeMap -> TypeMap
insert a (TypeMap mp) = TypeMap $ Map.insert (typeOf a) (toDyn a) mp


instance Ord TypeRep where
    compare a b = compare (splitTyConApp a) (splitTyConApp b)

instance Ord TyCon where
    compare a b = compare (tyConString a) (tyConString b)
