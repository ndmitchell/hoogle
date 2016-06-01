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


data Setting =
    RenameTag String String
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

    
