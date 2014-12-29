
module Util(fileSize) where

import System.IO

fileSize :: FilePath -> IO Int
fileSize file = withFile file ReadMode $ fmap fromIntegral . hFileSize
