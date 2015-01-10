
module Util(
    fileSize,
    pretty
    ) where

import System.IO
import Language.Haskell.Exts
import Data.List.Extra


fileSize :: FilePath -> IO Int
fileSize file = withFile file ReadMode $ fmap fromIntegral . hFileSize

pretty :: Pretty a => a -> String
pretty = trim . unwords . words . prettyPrint
