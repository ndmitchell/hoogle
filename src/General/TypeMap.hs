{-# LANGUAGE CPP #-}

module General.TypeMap(
    TypeMap, empty,
    lookup, insert, find
    ) where

import Prelude hiding (lookup)
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as Map

newtype TypeMap = TypeMap (Map.Map TypeRep Dynamic)


empty :: TypeMap
empty = TypeMap Map.empty


lookup :: Typeable a => TypeMap -> Maybe a
lookup (TypeMap mp) = res
    where res = fmap (fromJust . fromDynamic) $ Map.lookup (typeOf $ fromJust res) mp


find :: Typeable a => TypeMap -> a
find mp = res
    where res = fromMaybe (error msg) $ lookup mp
          msg = "General.TypeMap.find, couldn't find " ++ show (typeOf res)


insert :: Typeable a => a -> TypeMap -> TypeMap
insert a (TypeMap mp) = TypeMap $ Map.insert (typeOf a) (toDyn a) mp


#if __GLASGOW_HASKELL__ < 702
instance Ord TypeRep where
    compare a b = compare (splitTyConApp a) (splitTyConApp b)

instance Ord TyCon where
    compare a b = compare (tyConString a) (tyConString b)
#endif
