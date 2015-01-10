{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}

module DataTypes(writeTypes, searchTypes) where


import Language.Haskell.Exts
import System.IO.Extra
import System.FilePath

import Type
import Util


writeTypes :: Database -> [(Maybe Id, Items)] -> IO ()
writeTypes (Database file) xs = do
    writeFileBinary (file <.> "types") $ unlines
        [show i ++ " " ++ pretty t | (Just i, IDecl (TypeSig _ _ t)) <- xs]
    writeFileBinary (file <.> "alias") $ unlines
        [pretty t | (_, IDecl t@TypeDecl{}) <- xs]
    writeFileBinary (file <.> "instance") $ unlines
        [pretty t | (_, IDecl t@InstDecl{}) <- xs]


searchTypes :: Database -> Type -> IO [Id]
searchTypes = error "searchType"

{-
NEED TO ADD:

.arities file which lists the extent of each group of arities
.approx file, which gives the approximate matching sets

and a full search mechanism

-}
