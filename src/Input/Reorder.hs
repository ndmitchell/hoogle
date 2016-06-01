{-# LANGUAGE RecordWildCards #-}

module Input.Reorder(reorderItems) where

import Input.Item
import Input.Settings
import Data.List.Extra
import Data.Tuple.Extra


-- | Reorder items so the most popular ones are first, using reverse dependencies
reorderItems :: Settings -> (String -> Int) -> [(a, Item)] -> [(a, Item)]
reorderItems Settings{..} packageOrder xs =
    concatMap snd $ sortOn ((packageOrder &&& id) . fst) $ map rebase $ splitIPackage xs
    where
        refunc = map $ second $ \(x:xs) -> x : sortOn (itemName . snd) xs
        rebase (x, xs) = (x, concatMap snd $ sortOn (((negate . f) &&& id) . fst) $ refunc $ splitIModule xs)
            where f = reorderModule x
