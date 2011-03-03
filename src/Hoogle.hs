
-- | The Hoogle API. To perform a search you call 'search' with a 'Database' (obtained by 'loadDatabase') and a
--   'Query' (obtained by 'parseQuery').
module Hoogle(
    -- * Utility types
    TagStr(..), showTagText, showTagANSI, showTagHTML, showTagHTMLWith,
    H.ParseError(..),
    URL,
    H.Language(..),
    -- * Database
    Database, loadDatabase, saveDatabase, createDatabase, showDatabase,
    -- * Query
    Query, parseQuery, H.renderQuery,
    H.queryDatabases, H.queryPackages, H.querySetPackage,
    -- * Score
    Score, H.scoring,
    -- * Search
    Result(..), search, suggestions, completions
    ) where

import Hoogle.Store.All
import General.Base
import General.System

import Hoogle.Type.TagStr
import qualified Hoogle.DataBase.All as H
import qualified Hoogle.Query.All as H
import qualified Hoogle.Score.All as H
import qualified Hoogle.Search.All as H
import qualified Hoogle.Type.All as H
import qualified Hoogle.Language.Haskell as H

import Hoogle.Query.All(Query)
import Hoogle.Score.All(Score)


-- * Database

-- | A Hoogle database, containing a set of functions/items which can be searched. The 'Database' type is used
--   for a variety of purposes:
--
--   [Creation] A database is created by merging existing databases with the 'Monoid' instance and 'mappend',
--   or by creating a new 'Database' from an input file with 'createDatabase'.
--
--   [Serialization] A database is saved to disk with 'saveDatabase' and loaded from disk with 'loadDatabase'.
--
--   [Searching] A database is searched using 'search'.
newtype Database = Database [H.DataBase]

toDataBase (Database x) = H.combineDataBase x
fromDataBase x = Database [x]

instance Monoid Database where
    mempty = Database []
    mappend (Database xs) (Database ys) = Database $ xs ++ ys

instance Show Database where
    show = show . toDataBase


-- | Save a database to a file.
saveDatabase :: FilePath -> Database -> IO ()
saveDatabase file x = do
    performGC
    H.saveDataBase file $ toDataBase x


-- | Load a database from a file. If the database was not saved with the same version of Hoogle,
--   it will probably throw an error.
loadDatabase :: FilePath -> IO Database
loadDatabase = fmap fromDataBase . H.loadDataBase


-- | Create a database from an input definition. Source files for Hoogle databases are usually
--   stored in UTF8 format, and should be read using 'hSetEncoding' and 'utf8'.
createDatabase
    :: H.Language -- ^ Which format the input definition is in.
    -> [Database] -- ^ A list of databases which contain definitions this input definition relies upon (e.g. types, aliases, instances).
    -> String -- ^ The input definitions, usually with one definition per line, in a format specified by the 'Language'.
    -> ([H.ParseError], Database) -- ^ A pair containing any parse errors present in the input definition, and the database ignoring any parse errors.
createDatabase _ dbs src = (err, fromDataBase $ H.createDataBase xs res)
    where
        (err,res) = H.parseInputHaskell src
        xs = concat [x | Database x <- dbs]


-- | Show debugging information on some parts of the database. If the second argument
--   is 'Nothing' the whole database will be shown. Otherwise, the listed parts will be shown.
showDatabase :: Database -> Maybe [String] -> String
showDatabase x sects = concatMap (`H.showDataBase` toDataBase x) $ fromMaybe [""] sects


-- Hoogle.Query

-- | Parse a query for a given language, returning either a parse error, or a query.
parseQuery :: H.Language -> String -> Either H.ParseError Query
parseQuery _ = H.parseQuery


-- Hoogle.Search

-- Invariant: locations will not be empty
data Result = Result
    {locations :: [(URL, [(URL, String)])] -- your location, your parents
    ,self :: TagStr
    ,docs :: TagStr
    }

toResult :: H.Result -> (Score,Result)
toResult r@(H.Result ent view score) = (score, Result parents self docs)
    where
        self = H.renderResult r

        parents = map (second $ map f) $  H.entryLocations ent
        f = (H.entryURL &&& H.entryName) . fromOnce
        docs = H.renderDocs $ H.entryDocs ent


-- | Perform a search. The results are returned lazily.
search :: Database -> Query -> [(Score,Result)]
search (Database xs) q = map toResult $ H.search xs q

-- | Given a query and a database optionally give a list of what the user might have meant.
suggestions :: Database -> Query -> Maybe TagStr
suggestions (Database dbs) q = H.suggestQuery dbs q

-- | Given a query string and a database return a list of the possible completions for the search.
completions :: Database -> String -> [String]
completions x = H.completions (toDataBase x)
