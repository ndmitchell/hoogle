
-- Actual documentation results, stored per package, then reference by agregated bits
module Search.Value(
    valueCreate, valueSearch
    ) where

import Search.Type

-- First 16 bits is into a global table, rest is into this file
-- keep the global table and extend as necessary, reusing for same file name
valueCreate :: FilePath -> Package -> [a] -> IO [(Key, a)]
valueCreate = undefined


-- Look it up
valueSearch :: Key -> IO (Maybe a)
valueSearch = undefined
