
module Paths_hoogle where

import Data.Version.Extra

version :: Version
version = makeVersion [0,0]

getDataDir :: IO FilePath
getDataDir = return "."
