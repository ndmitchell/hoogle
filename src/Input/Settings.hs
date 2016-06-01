{-# LANGUAGE RecordWildCards #-}

-- | Module for reading settings files.
module Input.Settings(
    Settings(..), loadSettings
    ) where

import Data.List.Extra
import System.FilePath
import System.IO.Extra
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import Paths_hoogle


data Settings = Settings
    {renameTag :: String -> String -- ^ Rename a cabal tag
    }


-- | Fix bad names in the Cabal file.
loadSettings :: IO Settings
loadSettings = do
    dataDir <- getDataDir
    src <- readFileUTF8 $ dataDir </> "misc/settings.txt"
    let mp = Map.fromList $ mapMaybe (fmap (both trim) . stripInfix "=") $ lines src
    let renameTag x = Map.findWithDefault x x mp
    return Settings{..}

    
