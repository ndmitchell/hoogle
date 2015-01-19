
module Input.Type(
    Database(..),
    Item(..), Items(..), showItems, readItems,
    isIPackage, isIModule,
    URL, Documentation,
    Id(..)
    ) where

import Numeric
import Data.Tuple.Extra
import Language.Haskell.Exts
import General.Util

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
    = IDecl {fromIDecl :: Decl}
    | IKeyword {fromIKeyword :: String}
    | IPackage {fromIPackage :: String}
    | IModule {fromIModule :: String}
      deriving Show

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False


showItems :: Items -> String
showItems (IKeyword x) = "keyword " ++ x
showItems (IPackage x) = "package " ++ x
showItems (IModule x) = "module "
showItems (IDecl x) = pretty x


readItems :: String -> Maybe Items
readItems = undefined
