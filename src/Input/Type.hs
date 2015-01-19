{-# LANGUAGE ViewPatterns #-}

module Input.Type(
    Database(..),
    ItemEx(..), Item(..),
    showItem, prettyItem, readItem,
    isIPackage, isIModule,
    URL, Documentation,
    Id(..)
    ) where

import Numeric
import Data.Tuple.Extra
import Language.Haskell.Exts
import Data.List
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

data ItemEx = ItemEx
    {itemURL :: URL
    ,itemDocs :: Documentation
    ,itemParents :: [[(String, URL)]]
    ,itemItem :: Item
    } deriving Show

data Item
    = IDecl {fromIDecl :: Decl}
    | IKeyword {fromIKeyword :: String}
    | IPackage {fromIPackage :: String}
    | IModule {fromIModule :: String}
      deriving Show

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False


---------------------------------------------------------------------
-- ITEM AS STRING

showItem :: Item -> String
showItem (IKeyword x) = "@keyword " ++ x
showItem (IPackage x) = "@package " ++ x
showItem (IModule x) = "module " ++ x
showItem (IDecl x) = pretty x

prettyItem :: Item -> String
prettyItem (IKeyword x) = "keyword " ++ x
prettyItem (IPackage x) = "package " ++ x
prettyItem x = showItem x

readItem :: String -> Maybe Item
readItem (stripPrefix "@keyword " -> Just x) = Just $ IKeyword x
readItem (stripPrefix "@package " -> Just x) = Just $ IPackage x
readItem (stripPrefix "module " -> Just x) = Just $ IModule x
readItem x | ParseOk y <- parseDeclWithMode parseMode x = Just $ IDecl y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl a _ c d e f g) <- parseDeclWithMode parseMode $ "data " ++ x
    = Just $ IDecl $ DataDecl a NewType c d e f g
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ _ [GadtDecl s name _ ty] _) <- parseDeclWithMode parseMode $ "data Data where " ++ x
    = Just $ IDecl $ TypeSig s [name] ty
readItem o@('(':xs) -- tuple definitions
    | ")" `isPrefixOf` rest
    , ParseOk y <- parseDeclWithMode parseMode $ replicate (length com + 2) 'a' ++ drop 1 rest
    = Just $ IDecl $ f y
    where
        (com,rest) = span (== ',') xs
        f (TypeSig s [Ident _] ty) = TypeSig s [Ident $ '(':com++")"] ty
readItem _ = Nothing
