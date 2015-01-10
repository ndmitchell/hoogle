{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataNames(writeNames, searchNames) where

import Language.Haskell.Exts.Annotated
import System.IO.Extra
import System.FilePath

import Type

writeNames :: Database -> [(Maybe Id, Items)] -> IO ()
writeNames (Database file) xs = writeFileBinary (file <.> "names") $ unlines
    [show i ++ " " ++ prettyPrint name | (Just i, IDecl (TypeSig _ [name] _)) <- xs]

searchNames :: Database -> [String] -> IO [Id]
searchNames = undefined

