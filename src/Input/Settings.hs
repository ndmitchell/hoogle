{-# LANGUAGE RecordWildCards, PatternGuards, TemplateHaskell, CPP #-}


-- | Module for reading settings files.
module Input.Settings(
    Settings(..), loadSettings
    ) where

import Control.Exception (catch, throwIO)
import Data.List.Extra
import Data.Maybe
import Language.Haskell.TH.Syntax (lift, runIO)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.IO.Extra
import qualified Data.Map.Strict as Map
import Paths_hoogle


-- | Settings values. Later settings always override earlier settings.
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
    ,reorderModule :: String -> String -> Int
    }


readFileSettings :: FilePath -> String -> IO [Setting]
readFileSettings file backup = do
    src <- readFileUTF8 file `catch` \e ->
        if isDoesNotExistError e
            then return backup
            else throwIO e
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
#ifdef PROFILE
    -- profiling and TemplateHaskell don't play well
    let backup = ""
#else
    let backup = $(runIO (readFileUTF8 "misc/settings.txt") >>= lift)
#endif
    src <- readFileSettings (dataDir </> "misc/settings.txt") backup
    return $ createSettings src

createSettings :: [Setting] -> Settings
createSettings xs = Settings{..}
    where
        renameTag = \x -> fromMaybe x $ f x
            where f = literals [(a,b) | RenameTag a b <- xs]

        reorderModule = \pkg -> case f pkg of
                                    [] -> const 0
                                    xs -> let f = wildcards xs
                                          in \mod -> last $ 0 : f mod
            where f = wildcards [(a,(b,c)) | ReorderModule a b c <- xs]


---------------------------------------------------------------------
-- SPECIAL LOOKUPS

literals :: [(String, a)] -> String -> Maybe a
literals xs = \x -> Map.lookup x mp
    where mp = Map.fromList xs

wildcards :: [(String, a)] -> String -> [a]
wildcards xs x = [b | (a,b) <- xs, matchWildcard a x]

matchWildcard :: String -> String -> Bool
matchWildcard ['*'] ys = True -- special common case
matchWildcard ('*':xs) ys = any (matchWildcard xs) $ tails ys
matchWildcard (x:xs) (y:ys) = x == y && matchWildcard xs ys
matchWildcard [] [] = True
matchWildcard _ _ = False
