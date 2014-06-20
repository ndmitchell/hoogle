
-- | Each package has an index, plus there is one index that includes all index values.
--   Index is stored as a text file.
module Search.Index(
    indexCreate, indexMerge, indexLoad
    ) where

import Search.Type
import qualified Data.Map as Map

-- list of raw package-... things that go into an index
-- e.g. create [package-cmdargs,package-tagsoup] (Index author-neil)
-- boolean is does the index exist with search lumps
indexCreate :: FilePath -> Package -> [Package] -> Bool -> IO ()
indexCreate = undefined


-- merge the entries, so you have one global index
indexMerge :: FilePath -> [Package] -> Package -> IO ()
indexMerge = undefined


-- load an index from disk
indexLoad :: FilePath -> Package -> Map.Map String [String]
indexLoad = undefined
