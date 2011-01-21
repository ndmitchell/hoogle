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


-- | Given a query, return the list of packages that should be searched. Each package will be
--   the name of a database, without any file path or extension included.
queryDatabases :: Query -> [String]
queryDatabases x = if null ps then ["default"] else ps
    where ps = [p | PlusPackage p <- scope x]


-- | Return those packages which are explicitly excluded (paired with 'False')
--   or included (paired with 'True') in the query.
queryPackages :: Query -> [(Bool, String)]
queryPackages = concatMap f . scope
    where f (MinusPackage x) = [(False,x)]
          f (PlusPackage  x) = [(True ,x)]
          f _ = []

-- | Set the state of a package within a query. 'Nothing' means delete the package,
--   'Just' 'True' for add it, and 'Just' 'False' for remove it.
querySetPackage :: Maybe Bool -> String -> Query -> Query
querySetPackage b x q = q{scope= filter f (scope q) ++ [if b then PlusPackage x else MinusPackage x | Just b <- [b]]}
    where f (MinusPackage y) = x /= y
          f (PlusPackage  y) = x /= y
          f _ = True
