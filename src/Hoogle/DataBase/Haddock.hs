
module Hoogle.DataBase.Haddock(
    Haddock, newHaddock, renderHaddock
    ) where

import Data.TagStr
import Data.Binary.Defer
import Data.Binary.Defer.Vector


newtype Haddock = Haddock (Vector Char)


instance BinaryDefer Haddock where
    put (Haddock x) = put x
    get = get1 Haddock


newHaddock = Haddock . fromList


renderHaddock :: Haddock -> TagStr
renderHaddock (Haddock xs) = Str $ toList xs
