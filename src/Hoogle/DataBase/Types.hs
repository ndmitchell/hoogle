
module Hoogle.DataBase.Types(Types, saveTypes, loadTypes, followType) where

import System.IO
import Hoogle.TypeSig.All
import Hoogle.TextBase.All


-- free variables, type result
-- data TypeAlias = TypeAlias [String] TypeSig


data Types = Types


saveTypes :: Handle -> TextBase -> IO [String]
saveTypes hndl tb = return []


loadTypes :: Handle -> IO Types
loadTypes hndl = return Types


followType :: Types -> TypeSig -> Maybe TypeSig
followType _ a = Just a
