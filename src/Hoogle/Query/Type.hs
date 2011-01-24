{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Query.Type where

import General.Base
import Hoogle.Type.All

{-
GOALS OF THE QUERY REWRITE:

Query stores enough data to round trip perfectly

package:foo, module:foo, category:foo all work, all as a shortcut for +package:foo, ...
+package:foo, +package:bar
-package:foo, -package:bar
+bar is a shortcut for +package:bar
+Bar is a shortcut for +module:Bar

Type signatures need extending to contain _, * and ? - all of which mean wildcard

Need a way to "tweak" type signatures.

At it's heart, a query is a list of names, a type sig, and a list of scopes, in order.

Need to augment this with a list of extra information, such that when the extra information
is replayed, it does the same job.

Should really be [String], Maybe TypeSig, [Scope], those -> 

data QueryInfo = [String] (Maybe TypeSig) [Scope]
data Query = Query QueryInfo (QueryInfo -> String?)


"(a -" is assumed to be "(a -> _)"

"Ord a =" - is assumed to be "Ord a => _"

"(map > 1)" is total garbage... i think parse errors are still needed

Should try and autocomplete at the end where possible
-}


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
