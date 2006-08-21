
module Hoogle.DataBase.Items where

import Hoogle.TypeSig.All
import Hoogle.TextBase.All

import System.IO
import Data.List
import General.Binary
import Control.Monad


type ModuleName = [String]

data ItemType = ModuleType
              | ClassType
              | FuncType
              | AliasType
              | DataType
              | KeywordType
              deriving Enum


data DBItem = DBItem String (Maybe TypeSig)


-- take in (moduleId, item) -> [(itemId, item, dbitem)]
saveItems :: Handle -> [(Int, Item)] -> IO [(Int, Item, DBItem)]
saveItems hndl tb = mapM f tb
    where
        f (modu, x) = do
            i <- liftM fromInteger $ hTell hndl
            hPutInt hndl $ fromEnum $ getItem x
            hPutStr hndl $ show x -- hacky for now :)
            return (i, x, DBItem (getName x) (getType x))


getName :: Item -> String
getName x = case x of
        Class t -> f t
        Func n _ -> n
        TypeAlias a b -> f a
        Data _ t -> f t
        Keyword x -> x
        Module [x] -> x
    where
        f (TypeSig _ (TLit x)) = x
        f (TypeSig _ (TApp (TLit x) _)) = x

getItem :: Item -> ItemType
getItem x = case x of
    Class{} -> ClassType
    Func{} -> FuncType
    TypeAlias{} -> AliasType
    Data{} -> DataType
    Keyword{} -> KeywordType
    Module{} -> ModuleType


getType :: Item -> Maybe TypeSig
getType (Func _ t) = Just t
getType _ = Nothing

