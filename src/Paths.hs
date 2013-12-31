
module Paths_hoogle where

import Data.Version(Version(..))

version :: Version
version = Version {versionBranch = [4,2], versionTags = ["dev"]}

getDataDir, getLibDir :: IO FilePath
getDataDir = return "datadir"
getLibDir = return "C:\\Program Files\\Haskell\\hoogle-4.2\\ghc-7.6.3"
