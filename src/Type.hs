
module Type(
    Item(..), Items(..),
    isIPackage, isIModule,
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

data Item = Item
    {itemURL :: URL
    ,itemDocs :: Documentation
    ,itemParents :: [[(String, URL)]]
    ,itemItem :: Items
    } deriving Show

data Items
    = IDecl (Decl ())
    | IKeyword String
    | IPackage String
    | IModule String
    | ITag String String -- things like author/category/hackage
      deriving Show

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False
