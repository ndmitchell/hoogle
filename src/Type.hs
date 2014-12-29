
module Type(
    Section(..), Item(..),
    URL, Documentation,
    Id(..)
    ) where

import Numeric
import Language.Haskell.Exts.Annotated


type URL = String
type Documentation = String
newtype Id = Id Int

instance Show Id where
    show (Id x) = showHex x ""

data Section a = Section String String -- name, grouping thingy
               | Item a
                 deriving Show

data Item = IDecl (Decl ())
          | IKeyword String
          | IPackage String
          | IModule String
            deriving Show
