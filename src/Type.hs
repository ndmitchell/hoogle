
module Type(
    Database(..),
    Item(..), Items(..),
    isIPackage, isIModule,
    URL, Documentation,
    Id(..),
    Query(..), QTag(..)
    ) where

import Numeric
import Control.Monad
import Data.Monoid
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
    | ITag String String -- things like author/category/hackage
      deriving Show

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False


---------------------------------------------------------------------
-- QUERY

data Query = Query [QTag] [String] (Maybe Type) deriving Show

instance Monoid Query where
    mempty = Query [] [] Nothing
    mappend (Query x1 x2 x3) (Query y1 y2 y3) = Query (x1 ++ y1) (x2 ++ y2) (x3 `mplus` y3)

data QTag = QTag Bool String String deriving Show
