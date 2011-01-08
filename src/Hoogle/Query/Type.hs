{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Query.Type where

import Data.Maybe
import Data.Data
import Hoogle.Type.All


-- | A query, representing a user input.
data Query = Query
    {names :: [String]
    ,typeSig :: Maybe TypeSig
    ,scope :: [Scope]
    }
    deriving (Data,Typeable,Show,Eq)


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
