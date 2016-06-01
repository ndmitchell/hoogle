{-# LANGUAGE RecordWildCards #-}

-- | Module for reading settings files.
module Input.Settings(
    Settings(..), loadSettings
    ) where

import Data.List.Extra
import System.FilePath
import System.IO.Extra
import qualified Data.Map.Strict as Map
import Paths_hoogle


data Setting
    = -- | Given a Cabal tag/author rename it from the LHS to the RHS.
      --   If the RHS is blank, delete the tag.
      RenameTag String String
    | -- | Change the priority of a module. Given package name, module name, new priority.
      --   Use * for wildcard matches. All un-reordered modules are 0
      ReorderModule String String Int
    deriving Read


data Settings = Settings
    {renameTag :: String -> String -- ^ Rename a cabal tag
    }


readFileSettings :: FilePath -> IO [Setting]
readFileSettings file = do
    src <- readFileUTF8 file
    return $ concat $ zipWith f [1..] $ map trim $ lines src
    where
        f i s | null s = []
              | "--" `isPrefixOf` s = []
              | [(x,"")] <- reads s = [x]
              | otherwise = error $ file ++ ":" ++ show i ++ ": Failure to parse, got: " ++ s



-- | Fix bad names in the Cabal file.
loadSettings :: IO Settings
loadSettings = do
    dataDir <- getDataDir
    src <- readFileSettings $ dataDir </> "misc/settings.txt"
    let mp = Map.fromList [(a,b) | RenameTag a b <- src]
    let renameTag x = Map.findWithDefault x x mp
    return Settings{..}

    
