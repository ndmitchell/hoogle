{-# LANGUAGE PatternGuards #-}

-- FIXME: Should be moved inside TextBase
-- | Code to figure out default URL's based on Hackage/Haddock semantics
module Hoogle.Item.URL(defaultPackageURL, defaultModuleURL, defaultEntryURL) where

import General.Code
import Hoogle.Item.Item
import Data.Binary.Defer.Index


url base def given
    | any (`isPrefixOf` use) ["http:","https:"] = use
    | otherwise = base ++ use
    where use = if null given then def else given


defaultPackageURL :: Package -> Package
defaultPackageURL x = x{packageURL = url "" def $ packageURL x}
    where def = "http://hackage.haskell.org/packages/" ++ packageName x ++ "/"


defaultModuleURL :: Module -> Module
defaultModuleURL x = x{moduleURL = url base def $ moduleURL x}
    where base = packageURL (fromLink $ modulePackage x)
          def =  "docs/" ++ intercalate "-" (moduleName x) ++ ".html"


defaultEntryURL :: Entry -> Entry
defaultEntryURL x = x {- x{entryURL = res}
    where
        y = entryURL x
        res = case entryType x of
            EntryPackage -> url "" (packageURL $ fromLink $ entryPackage x) y
            EntryModule | Just m <- entryModule x -> url (packageURL $ fromLink $ entryPackage x) (moduleURL $ fromLink m) y
            EntryOther | Just m <- entryModule x -> url (moduleURL $ fromLink m) ("#v:" ++ entryName x) y
            _ -> ""
-}
