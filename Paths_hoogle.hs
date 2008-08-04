{-|
    I would much rather put this module hidden away in src
    but then Cabal will always use it in preference to the
    auto-generated one.
-}

module Paths_hoogle where

import Data.Version(Version(..))

version :: Version
version = Version {versionBranch = [4,0], versionTags = ["manual"]}

getDataDir :: IO FilePath
getDataDir = return ""
