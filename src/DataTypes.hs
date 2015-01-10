{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataTypes(writeTypes, searchTypes) where


import Language.Haskell.Exts.Annotated
import System.IO.Extra
import Data.List.Extra
import System.FilePath

import Type


writeTypes :: Database -> [(Maybe Id, Items)] -> IO ()
writeTypes (Database file) xs = writeFileBinary (file <.> "types") $ unlines
    [show i ++ " " ++ trimStart (unwords $ words $ prettyPrint $ fmap (const noLoc) t) | (Just i, IDecl (TypeSig _ _ t)) <- xs]


searchTypes :: Database -> Type () -> IO [Id]
searchTypes = error "searchType"

