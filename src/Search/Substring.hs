{-# LANGUAGE ScopedTypeVariables #-}

-- | Default substring searching
module Search.Substring(
    substringCreate, substringMerge, substringSearch
    ) where

import Search.Type
import General.Base
import System.FilePath


substringCreate :: FilePath -> Package -> [(Key, String)] -> IO ()
substringCreate dir p items = do
    writeFile (dir </> show p <.> "substring") $ show items
    -- write out a trie, case-insensitive, where prefix matches and non-prefix are separated


substringMerge :: FilePath -> [Package] -> IO ()
substringMerge _ _ = error "substringMerge"
    -- merge the trie's


substringSearch :: FilePath -> Package -> String -> IO [(Double, Key)]
substringSearch dir p x = do
    src <- read <$> readFile (dir </> show p <.> "substring")
    let f (k :: Key, s :: String) 
            | lower x == lower s = Just (1, k)
            | lower x `isPrefixOf` lower s = Just (0.5, k)
            | lower x `isInfixOf` lower s = Just (0, k)
            | otherwise = Nothing
    return $ reverse $ sortBy (compare `on` fst) $ mapMaybe f src
    -- search from multiple tries, merge so perfect comes first, then prefix, then non-prefix ones
    -- use order to implement priority, so have base package, then platform(-base), then stackage(-platform), then all(-stackage)
