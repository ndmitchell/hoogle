{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Query.Type where

import General.Base
import Hoogle.Type.All


-- | A query, representing a user input.
data Query = Query
    {names :: [String]
    ,typeSig :: Maybe TypeSig
    ,scope :: [Scope]
    ,exactSearch :: Maybe ItemKind
    ,invertResults :: Bool
    }
    deriving (Data,Typeable,Show,Eq)

instance Monoid Query where
    mempty = Query [] Nothing [] Nothing False
    mappend (Query x1 x2 x3 x4 x5) (Query y1 y2 y3 y4 y5) =
        Query (x1++y1) (x2 `mplus` y2) (x3++y3) (merge x4 y4) (x5 || y5)
      where merge Nothing Nothing = Nothing
            merge (Just x) Nothing = Just x
            merge Nothing (Just y) = Just y
            merge (Just UnclassifiedItem) (Just y) = Just y
            merge (Just x) (Just UnclassifiedItem) = Just x
            merge (Just x) (Just _) = Just x

data Scope = Scope Bool Category String deriving (Data,Typeable,Show,Eq)
data Category = Module | Package deriving (Data,Typeable,Show,Eq)


-- | Given a query, return the list of packages that should be searched. Each package will be
--   the name of a database, without any file path or extension included.
queryDatabases :: Query -> [String]
queryDatabases q = if null ps then ["default"] else ps
    where ps = [p | Scope True Package p <- scope q]


-- | Return those packages which are explicitly excluded (paired with 'False')
--   or included (paired with 'True') in the query.
queryPackages :: Query -> [(Bool, String)]
queryPackages q = [(b,s) | Scope b Package s <- scope q]


-- | Set the state of a package within a query. 'Nothing' means delete the package,
--   'Just' 'True' for add it, and 'Just' 'False' for remove it.
querySetPackage :: Maybe Bool -> String -> Query -> Query
querySetPackage b x q = q{scope = filter f (scope q) ++ [Scope b Package x | Just b <- [b]]}
    where f (Scope _ Package y) = x /= y
          f _ = True
