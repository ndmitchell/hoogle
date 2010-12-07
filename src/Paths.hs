
module Paths_hoogle where

import Data.Version(Version(..))

version :: Version
version = Version {versionBranch = [4,1], versionTags = ["dev"]}

getDataDir, getLibDir :: IO FilePath
getDataDir = return "datadir"
getLibDir = return "C:\\Program Files\\Haskell\\hoogle-4.1\\ghc-6.12.3"
