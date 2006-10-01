
module Hoogle.DataBase.Alias(Alias, saveAlias, loadAlias, followAlias) where

import System.IO
import Hoogle.TypeSig.All
import Hoogle.TextBase.All
import General.All


-- free variables, type result
-- data TypeAlias = TypeAlias [String] TypeSig


data Alias = Alias


saveAlias :: Handle -> TextBase -> IO [Response]
saveAlias hndl tb = return []


loadAlias :: Handle -> IO Alias
loadAlias hndl = return Alias


followAlias :: Alias -> TypeSig -> Maybe TypeSig
followAlias _ a = Just a
