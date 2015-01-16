
module Paths_hogle where

import Data.Version(Version(..))

version :: Version
version = Version {versionBranch = [0,0], versionTags = ["dev"]}

getDataDir :: IO FilePath
getDataDir = return "data"
