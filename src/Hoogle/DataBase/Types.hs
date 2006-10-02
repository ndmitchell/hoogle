
module Hoogle.DataBase.Types(saveTypes, searchTypes) where

import System.IO

import General.All
import Hoogle.Common.All
import Hoogle.TypeSig.All


saveTypes :: Handle -> [Item] -> IO [Response]
saveTypes _ _ = return []



searchTypes :: Handle -> TypeSig -> IO [Result]
searchTypes _ _ = return []
