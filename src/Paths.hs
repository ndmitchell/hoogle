
module Paths_hoogle where

import Data.Version(Version(..))

version :: Version
version = Version {versionBranch = [0,0], versionTags = ["dev"]}

getDataDir :: IO FilePath
getDataDir = return "data"
