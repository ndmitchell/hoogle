
module Type(
    Tagged(..), ItemEx(..), Item(..),
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

data Tagged a
    = Tagged String String -- tag name, grouping critera
    | Item a
      deriving Show


data ItemEx = ItemEx
    {itemURL :: URL
    ,itemDocs :: Documentation
    ,itemParents :: [[(String, URL)]]
    ,itemItem :: Item
    } deriving Show

data Item
    = IDecl (Decl ())
    | IKeyword String
    | IPackage String
    | IModule String
      deriving Show
