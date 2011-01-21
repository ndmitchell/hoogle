{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Query.Type where

import General.Base
import Hoogle.Type.All


-- | A query, representing a user input.
data Query = Query
    {names :: [String]
    ,typeSig :: Maybe TypeSig
    ,scope :: [Scope]
    }
    deriving (Data,Typeable,Show,Eq)

instance Monoid Query where
    mempty = Query [] Nothing []
    mappend (Query x1 x2 x3) (Query y1 y2 y3) =
        Query (x1++y1) (x2 `mplus` y2) (x3++y3)


data Scope = PlusPackage  String
           | MinusPackage String
           | PlusModule  [String]
           | MinusModule [String]
           deriving (Eq, Show, Read, Data, Typeable)

isPlusModule  (PlusModule  _) = True; isPlusModule  _ = False
isMinusModule (MinusModule _) = True; isMinusModule _ = False
