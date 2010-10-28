{-# LANGUAGE PatternGuards #-}

-- | Code to figure out default URL's based on Hackage/Haddock semantics
module Hoogle.Item.URL where

import General.Code
import Hoogle.Item.Item
import Numeric
import Data.Binary.Defer.Index


a `orElse` b = if null a then b else a


defaultPackageURL :: Package -> Package
defaultPackageURL x = x{packageURL = packageURL x `orElse` def}
    where def = "http://hackage.haskell.org/packages/" ++ packageName x


defaultModuleURL :: Module -> Module
defaultModuleURL x = x{moduleURL = moduleURL x `orElse` def}
    where def = packageURL (fromLink $ modulePackage x) ++ "/docs/" ++ intercalate "-" (moduleName x) ++ ".html"

defaultEntryURL :: Entry -> Entry
defaultEntryURL x = x{entryURL = entryURL x `orElse` def}
    where
        def = case entryType x of
            EntryPackage -> packageURL $ fromLink $ entryPackage x
            EntryModule | Just m <- entryModule x -> moduleURL $ fromLink m
            EntryOther | Just m <- entryModule x -> moduleURL (fromLink m) ++ "#v:" ++ entryName x
            EntryKeyword -> "http://www.haskell.org/haskellwiki/Keywords#" ++ concatMap f (entryName x)
            _ -> ""

        f x | isAlpha x || x `elem` "_-:" = [x]
            | otherwise = '.' : map toUpper (showHex (ord x) "")
