{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Query.Type2 where

import General.Base
import Hoogle.Type.All

data Scope = Package | Module

data Query
    = ByName String
    | ByType TypeSig
    | Scope Bool Scope String


-- | Given a query, return the list of packages that should be searched. Each package will be
--   the name of a database, without any file path or extension included.
queryDatabases :: [Query] -> [String]
queryDatabases q = if null ps then ["default"] else ps
    where ps = [p | Scope True Package p <- q]


-- | Return those packages which are explicitly excluded (paired with 'False')
--   or included (paired with 'True') in the query.
queryPackages :: [Query] -> [(Bool, String)]
queryPackages q = [(b,s) | Scope b Package s <- q]


-- | Set the state of a package within a query. 'Nothing' means delete the package,
--   'Just' 'True' for add it, and 'Just' 'False' for remove it.
querySetPackage :: Maybe Bool -> String -> [Query] -> [Query]
querySetPackage b x q = filter f q ++ [Scope b Package x | Just b <- [b]]
    where f (Scope _ Package y) = x /= y
          f _ = True
