
-- Actual documentation results, stored per package, then reference by agregated bits
module Search.Value(
    valueCreate, valueSearch
    ) where

import Search.Type
import General.Base


-- First 16 bits is into a global table, rest is into this file
-- keep the global table and extend as necessary, reusing for same file name
valueCreate :: Show a => FilePath -> Package -> [a] -> IO [Key]
valueCreate dir p xs = return $ map (Key . show) xs


-- Look it up
valueSearch :: Read a => FilePath -> Key -> IO (Maybe a)
valueSearch _ (Key k) = return $ Just $ read k
