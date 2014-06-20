
-- | Like Substring but for whole words, used for documentation.
--   Just sort all the words, and do a binary search.
module Search.Word(
    wordCreate, wordMerge, wordSearch
    ) where

import Search.Type

wordCreate :: FilePath -> Package -> [(Key, String)] -> IO ()
wordCreate = undefined


wordMerge :: FilePath -> [Package] -> IO ()
wordMerge = undefined


wordSearch :: FilePath -> [Package] -> String -> [Key]
wordSearch = undefined
