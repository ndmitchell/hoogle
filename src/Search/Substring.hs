
-- | Default substring searching
module Search.Substring(
    substringCreate, substringMerge, substringSearch
    ) where

import Search.Type


substringCreate :: FilePath -> Package -> [(Key, String)] -> IO ()
substringCreate = undefined
    -- write out a trie, case-insensitive, where prefix matches and non-prefix are separated


substringMerge :: FilePath -> [Package] -> IO ()
substringMerge = undefined
    -- merge the trie's


substringSearch :: FilePath -> [Package] -> String -> IO [Key]
substringSearch = undefined
    -- search from multiple tries, merge so perfect comes first, then prefix, then non-prefix ones
    -- use order to implement priority, so have base package, then platform(-base), then stackage(-platform), then all(-stackage)
