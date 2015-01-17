
module Input.Type(
    Database(..),
    Item(..), Items(..),
    isIPackage, isIModule,
    URL, Documentation,
    Id(..)
    ) where

import Numeric
import Data.Tuple.Extra
import Language.Haskell.Exts

newtype Database = Database FilePath

---------------------------------------------------------------------
-- DATABASE

type URL = String
type Documentation = String
newtype Id = Id Int deriving (Eq,Ord)

instance Show Id where
    show (Id x) = showHex x ""

instance Read Id where
    readsPrec _ = map (first Id) . readHex

data Item = Item
    {itemURL :: URL
    ,itemDocs :: Documentation
    ,itemParents :: [[(String, URL)]]
    ,itemItem :: Items
    } deriving Show

data Items
    = IDecl Decl
    | IKeyword String
    | IPackage String
    | IModule String
      deriving Show

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False
