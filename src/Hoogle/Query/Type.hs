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

-- | Test if a query will result in a search being performed. A query which lists only scopes
--   (e.g. @+base@) will still be reported is blank.
isBlankQuery :: Query -> Bool
isBlankQuery query = null (names query) && isNothing (typeSig query)

blankQuery :: Query
blankQuery = Query [] Nothing []


data Scope = PlusPackage  String
           | MinusPackage String
           | PlusModule  [String]
           | MinusModule [String]
           deriving (Eq, Show, Read, Data, Typeable)

isPlusModule  (PlusModule  _) = True; isPlusModule  _ = False
isMinusModule (MinusModule _) = True; isMinusModule _ = False
