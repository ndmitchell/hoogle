{-# LANGUAGE RecordWildCards #-}

module Input.Reorder(reorderItems) where

import Input.Item
import Input.Settings
import Data.List.Extra
import Data.Tuple.Extra
import General.Util
import General.Str


-- | Reorder items so the most popular ones are first, using reverse dependencies
reorderItems :: Settings -> (PkgName -> Int) -> [(a, Item)] -> [(a, Item)]
reorderItems Settings{..} packageOrder xs =
    concatMap snd $ sortOn ((packageOrder &&& id) . fst) $ map rebase $ splitIPackage xs
    where
        refunc = map $ second $ \(x:xs) -> x : sortOn (itemName . snd) xs
        rebase (x, xs) = (x, concatMap snd $ sortOn (((negate . f . strUnpack) &&& id) . fst) $ refunc $ splitIModule xs)
            where f = reorderModule (strUnpack x)
