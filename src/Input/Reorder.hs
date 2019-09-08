{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Input.Reorder(reorderItems) where

import Input.Item
import Input.Settings
import Data.List.Extra
import Data.Tuple.Extra
import General.Util
import General.Str


pkgGhc :: PkgName
pkgGhc = strPack "ghc"

packageOrderHacks :: (PkgName -> Int) -> PkgName -> Int
-- 'ghc' is the canonical module that both 'ghc-lib-parser' and 'ghc-lib' copy from, so better to pick that
-- even though ghc-lib-* are used more on Stackage (but a lot less on Hackage)
packageOrderHacks f x | x == pkgGhc = min (f x) $ min (f $ strPack "ghc-lib-parser") (f $ strPack "ghc-lib") - 1
packageOrderHacks f x = f x


-- | Reorder items so the most popular ones are first, using reverse dependencies.
--   Low numbers for the PkgName function mean the package is more popular.
reorderItems :: Settings -> (PkgName -> Int) -> [(a, Item)] -> [(a, Item)]
reorderItems Settings{..} packageOrder xs =
    concatMap snd $ sortOn ((packageOrderHacks packageOrder &&& id) . fst) $ map rebase $ splitIPackage xs
    where
        refunc = map $ second $ \(x:xs) -> x : sortOn (itemName . snd) xs
        rebase (x, xs) = (x, concatMap snd $ sortOn (((negate . f . strUnpack) &&& id) . fst) $ refunc $ splitIModule xs)
            where f = reorderModule (strUnpack x)
