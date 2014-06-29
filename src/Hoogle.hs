
-- | The Hoogle API. To perform a search you call 'search' with a 'Database' (obtained by 'loadDatabase') and a
--   'Query' (obtained by 'parseQuery').
module Hoogle(
    -- * Utility types
    TagStr(..), showTagText, showTagANSI, showTagHTML, showTagHTMLWith,
    H.ParseError(..),
    URL,
    H.Language(..),
    -- * Database
    Database, loadDatabase, saveDatabase, createDatabase, mergeDatabase, showDatabase,
    defaultDatabaseLocation,
    -- * Query
    Query, parseQuery, H.renderQuery,
    H.queryDatabases, H.queryPackages, H.querySetPackage,
    -- * Score
    Score, H.scoring,
    -- * Search
    Result(..), search, suggestions, completions, queryExact, H.ItemKind(..)
    ) where

import Hoogle.Store.All
import General.Base
import General.System
import System.FilePath
import Hoogle.DataBase2.Type
import Hoogle.DataBase2.Str
import System.IO.Unsafe
import Paths_hoogle

import Hoogle.Type.TagStr
import qualified Hoogle.DataBase.All as H
import qualified Hoogle.Query.All as H
import qualified Hoogle.Score.All as H
import qualified Hoogle.Search.All as H
import qualified Hoogle.Type.All as H
import qualified Hoogle.Language.Haskell as H

import Hoogle.Query.All(Query, exactSearch)
import Hoogle.Score.All(Score)

-- Turn on the new index/search pieces
new = False
new2 = False

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
newtype Database = Database [(FilePath, H.DataBase)]

toDataBase (Database x) = H.combineDataBase $ map snd x

instance NFData Database where
    rnf (Database a) = rnf a

instance Monoid Database where
    mempty = Database []
    mappend (Database xs) (Database ys) = Database $ xs ++ ys

instance Show Database where
    show = show . toDataBase


-- | Save a database to a file.
saveDatabase :: FilePath -> Database -> IO ()
saveDatabase file x@(Database xs) = do
    performGC
    H.saveDataBase file $ toDataBase x
    when new $ do
        performGC
        mergeStr [x <.> "str" | (x,_) <- xs] (file <.> "str")


mergeDatabase :: [FilePath] -> FilePath -> IO ()
mergeDatabase src out = do
    x <- mapM loadDatabase src
    saveDatabase out $ mconcat x


-- | Load a database from a file. If the database was not saved with the same version of Hoogle,
--   it will probably throw an error.
loadDatabase :: FilePath -> IO Database
loadDatabase x = do db <- H.loadDataBase x; return $ Database [(x, db)]


defaultDatabaseLocation :: IO FilePath
defaultDatabaseLocation = getDataDir


-- | Create a database from an input definition. Source files for Hoogle databases are usually
--   stored in UTF8 format, and should be read using 'hSetEncoding' and 'utf8'.
createDatabase
    :: H.HackageURL
    -> H.Language -- ^ Which format the input definition is in.
    -> [Database] -- ^ A list of databases which contain definitions this input definition relies upon (e.g. types, aliases, instances).
    -> String -- ^ The input definitions, usually with one definition per line, in a format specified by the 'Language'.
    -> FilePath -- ^ Output file
    -> IO [H.ParseError] -- ^ A list of any parse errors present in the input definition that were skipped.
createDatabase url _ dbs src out = do
    let (err,res) = H.parseInputHaskell url src
    let xs = concat [map snd x | Database x <- dbs]
    let db = H.createDataBase xs res
    performGC
    items <- H.saveDataBase out db
    -- don't build .str for .dep files
    when (new && takeExtension out == ".hoo") $ do
        createStr' (newPackage $ takeBaseName out) (map (Pos *** fromOnce) items) (out <.> "str")
    when (new2 && takeExtension out == ".hoo") $ do
        items <- fmap (map snd) $ H.saveDataBase (dropExtension out <.> "idx.hoo") $ H.createDataBaseEntries res
        items <- return $ flip map items $ unsafeFmapOnce $ \e -> e{H.entryLocations = map (first $ const "") $ H.entryLocations e, H.entryName="", H.entryText=mempty, H.entryDocs=mempty}
        H.saveDataBase (dropExtension out <.> "str.hoo") $ H.createDataBaseText items
        H.saveDataBase (dropExtension out <.> "typ.hoo") $ H.createDataBaseType xs res items
        return ()
    return err


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
    ,self :: TagStr -- ^ Rendered view for the entry, including name/keywords/type as appropriate, colors matching 'renderQuery'
    ,docs :: TagStr -- ^ Documentation for the entry
    }
    deriving (Eq, Show)

toResult :: H.Result -> (Score,Result)
toResult r@(H.Result ent view score) = (score, Result parents self docs)
    where
        self = H.renderResult r

        parents = map (second $ map f) $  H.entryLocations ent
        f = (H.entryURL &&& H.entryName) . fromOnce
        docs = H.renderDocs $ H.entryDocs ent


-- | Perform a search. The results are returned lazily.
search :: Database -> Query -> [(Score,Result)]
search (Database xs@((root,_):_)) (H.Query [name] Nothing scopes Nothing False) | new && all simple scopes =
    unsafePerformIO $ map toResult <$> searchStr' resolve (map fst xs) name
    where resolve pkg pos = runSGetAt pos (takeDirectory root </> pkg <.> "hoo") get
          simple (H.Scope a b _) = a && b == H.Package
search (Database xs) q = map toResult $ H.search (map snd xs) q

-- | Given a query and a database optionally give a list of what the user might have meant.
suggestions :: Database -> Query -> Maybe TagStr
suggestions (Database dbs) q = H.suggestQuery (map snd dbs) q

-- | Given a query string and a database return a list of the possible completions for the search.
completions :: Database -> String -> [String]
completions x = H.completions (toDataBase x) -- FIXME: Doing a merge on completions? Bad idea.

-- | Given a query, set whether it is an exact query.
queryExact :: Maybe H.ItemKind -> Query -> Query
queryExact kind q = q { exactSearch = kind }
