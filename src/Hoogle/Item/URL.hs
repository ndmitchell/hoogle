
module Hoogle.Item.URL where

import General.Code
import Hoogle.Item.Item
import Web.Text(escapeHTML) -- TODO: Shouldn't be importing this!
import Numeric
import Data.Binary.Defer.Index



entryURL :: Entry -> String
entryURL e@Entry{entryType=EntryModule} = entryModuleURL e
entryURL e@Entry{entryType=EntryPackage} = entryPackageURL e
entryURL e@Entry{entryType=EntryOther} = entryModuleURL e ++ "#v:" ++ escapeHTML (entryName e)
entryURL e@Entry{entryType=EntryKeyword} =
        "http://www.haskell.org/haskellwiki/Keywords#" ++ concatMap f (entryName e)
    where
        f x | isAlpha x || x `elem` "_-:" = [x]
            | otherwise = '.' : map toUpper (showHex (ord x) "")


entryModuleURL :: Entry -> String
entryModuleURL Entry{entryModule=Just m, entryPackage=p} | pkg /= "" =
        pkg ++ concat (intersperse "-" name) ++ ".html"
    where
        name = moduleName $ fromLink m
        pkg = haddockURL $ fromLink p
entryModuleURL _ = ""


entryPackageURL :: Entry -> String
entryPackageURL e = if a == "" then b else a
    where Package{hackageURL=a, haddockURL=b} = fromLink $ entryPackage e
