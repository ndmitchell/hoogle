{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Query.Type where

import Data.Maybe
import Data.Data
import Data.Generics.UniplateOn

import General.Code
import Hoogle.Type.All


isBlankQuery :: Query -> Bool
isBlankQuery query = null (names query) && isNothing (typeSig query)


data Query = Query {
        scope :: [Scope],
        names :: [String],
        typeSig :: Maybe TypeSig
    }
    deriving (Data,Typeable,Show)


emptyQuery = Query [] [] Nothing


instance Eq Query where
    (Query a1 b1 c1) == (Query a2 b2 c2) =
        and [a1 `setEq` a2, b1 `setEq` b2, c1 == c2]


data Scope = PlusPackage  String
           | MinusPackage String
           | PlusModule  [String]
           | MinusModule [String]
           deriving (Eq, Show, Read, Data, Typeable)

isPlusModule  (PlusModule  _) = True; isPlusModule  _ = False
isMinusModule (MinusModule _) = True; isMinusModule _ = False


transformQueryType :: (Type -> Type) -> Query -> Query
transformQueryType f q = q{typeSig = maybe Nothing (Just . transformOn onTypeSig f) (typeSig q)}

universeQueryType :: Query -> [Type]
universeQueryType = maybe [] (universeOn onTypeSig) . typeSig
