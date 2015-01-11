
module Util(
    fileSize,
    pretty,
    fromName, fromTyVarBind
    ) where

import System.IO
import Language.Haskell.Exts
import Data.List.Extra


fileSize :: FilePath -> IO Int
fileSize file = withFile file ReadMode $ fmap fromIntegral . hFileSize

pretty :: Pretty a => a -> String
pretty = trim . unwords . words . prettyPrint

fromName :: Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

fromTyVarBind :: TyVarBind -> Name
fromTyVarBind (KindedVar x _) = x
fromTyVarBind (UnkindedVar x) = x
