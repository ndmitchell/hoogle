{-# LANGUAGE TupleSections #-}

-- | The Hoogle API. To perform a search you call 'search' with a 'Database' (obtained by 'loadDatabase') and a
--   'Query' (obtained by 'parseQuery').
module Hoogle(
    -- * Utility types
    TagStr(..), showTagText, showTagANSI, showTagHTML, showTagHTMLWith,
    H.ParseError(..),
    URL,
    H.Language(..),
    -- * Database
    Package(..), packageCreate, packageAlias, packageMerge, packageShow, packageReindex,
    -- * Query
    Query, parseQuery, H.renderQuery,
    H.queryDatabases, H.queryPackages, H.querySetPackage,
    -- * Score
    Score, H.scoring,
    -- * Search
    Result(..), search, suggestions, completions, queryExact, H.ItemKind(..)
    ) where

import Hoogle.Store.All
import Search.All
import General.Base
import General.System
import System.FilePath
import Hoogle.DataBase2.Type hiding (Package)
import qualified Hoogle.DataBase2.Type as HH
import Hoogle.DataBase2.Str
import System.IO.Unsafe

import Hoogle.Type.TagStr
import qualified Hoogle.DataBase.All as H
import qualified Hoogle.Query.All as H
import qualified Hoogle.Score.All as H
import qualified Hoogle.Search.All as H
import qualified Hoogle.Type.All as H
import qualified Hoogle.Language.Haskell as H

import Hoogle.Query.All(Query, exactSearch)
import Hoogle.Score.All(Score)


packageReindex :: FilePath -> IO ()
packageReindex = undefined

-- | Save a database to a file.
packageAlias :: FilePath -> [Package] -> Package -> IO ()
packageAlias = error "packageAlias" {- file x@(Database xs) = do
    performGC
    H.saveDataBase file $ toDataBase x
    when new $ do
        performGC
        mergeStr [x <.> "str" | (x,_) <- xs] (file <.> "str") -}


packageMerge :: FilePath -> [Package] -> Package -> IO ()
packageMerge = error "packageMerge" {- src out = do
    x <- mapM loadDatabase src
    saveDatabase out $ mconcat x -}


-- | Create a database from an input definition. Source files for Hoogle databases are usually
--   stored in UTF8 format, and should be read using 'hSetEncoding' and 'utf8'.
packageCreate
    :: FilePath
    -> H.Language -- ^ Which format the input definition is in.
    -> H.HackageURL
    -> [Package] -- ^ A list of databases which contain definitions this input definition relies upon (e.g. types, aliases, instances).
    -> String -- ^ The input definitions, usually with one definition per line, in a format specified by the 'Language'.
    -> Package -- ^ Output file
    -> IO [H.ParseError] -- ^ A list of any parse errors present in the input definition that were skipped.
packageCreate dir _ url deps src out = do
    let (err,(fact,item)) = H.parseInputHaskell url src
    items <- (`zip` item) <$> valueCreate dir out (map itemResult item)
    substringCreate dir out $ map (second H.itemKey) items
    signatureCreate dir out deps fact
        (mapMaybe (\(a,b) -> fmap (a,) $ H.itemType b) items)
    indexCreate dir out [out] True
    return err

itemResult :: H.TextItem -> Result
itemResult i = Result [] (Str $ H.itemName i) mempty


-- | Show debugging information on some parts of the database. If the second argument
--   is 'Nothing' the whole database will be shown. Otherwise, the listed parts will be shown.
packageShow :: FilePath -> Package -> Maybe [String] -> String
packageShow x sects = error "packageShow" -- concatMap (`H.showDataBase` toDataBase x) $ fromMaybe [""] sects


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
    deriving (Eq, Show, Read)

toResult :: H.Result -> (Score,Result)
toResult r@(H.Result ent view score) = (score, Result parents self docs)
    where
        self = H.renderResult r

        parents = map (second $ map f) $  H.entryLocations ent
        f = (H.entryURL &&& H.entryName) . fromOnce
        docs = H.renderDocs $ H.entryDocs ent


-- | Perform a search.
search :: FilePath -> Query -> IO [Result]
search dir (H.Query [] Nothing _ _ _) = return []
search dir (H.Query names typ sc _ _) = do
    (pkgs, filt) <- scopes dir sc
    case typ of
        Just t -> do
            res <- fmap merge $ ioFor pkgs $ \pkg -> signatureSearch dir pkg (error "convert" typ)
            ioCatMaybes res $ valueSearch dir . fst
        Nothing -> do
            name:names <- return names
            res <- fmap merge $ forM pkgs $ \pkg -> substringSearch dir pkg name
            ioCatMaybes res $ valueSearch dir

ioFor :: [a] -> (a -> IO b) -> IO [b]
ioFor [] f = return []
ioFor (x:xs) f =
    do x <- f x; xs <- unsafeInterleaveIO $ ioFor xs f; return $ x:xs


ioCatMaybes :: [a] -> (a -> IO (Maybe b)) -> IO [b]
ioCatMaybes [] f = return []
ioCatMaybes (x:xs) f =
    do x <- f x; xs <- unsafeInterleaveIO $ ioCatMaybes xs f; return $ maybeToList x ++ xs


merge :: Ord k => [[(k, v)]] -> [v]
merge  xs | null ys = []
          | otherwise = snd (head me) : merge (pre ++ [tail me] ++ post)
    where ys = filter (not . null) xs
          i = maximumBy (compare `on` (\i -> fst $ head $ ys !! i)) [0 .. length ys-1]
          (pre,me:post) = splitAt i ys


scopes :: FilePath -> [H.Scope] -> IO ([Package], Result -> Bool)
scopes dir [H.Scope True H.Package x] = return ([Package x], const True)
scopes dir sc = error "scopes"


{-
search dir (Database xs@((root,_):_)) (H.Query [name] Nothing scopes Nothing False) | new && all simple scopes =
    unsafePerformIO $ map toResult <$> searchStr' resolve (map fst xs) name
    where resolve pkg pos = runSGetAt pos (takeDirectory root </> pkg <.> "hoo") get
          simple (H.Scope a b _) = a && b == H.Package
search (Database xs) q = map toResult $ H.search (map snd xs) q
-}

-- | Given a query and a database optionally give a list of what the user might have meant.
suggestions :: FilePath -> Query -> Maybe TagStr
suggestions _ _ = Nothing -- (Database dbs) q = H.suggestQuery (map snd dbs) q

-- | Given a query string and a database return a list of the possible completions for the search.
completions :: FilePath -> String -> [String]
completions = error "completions" -- x = H.completions (toDataBase x) -- FIXME: Doing a merge on completions? Bad idea.

-- | Given a query, set whether it is an exact query.
queryExact :: Maybe H.ItemKind -> Query -> Query
queryExact kind q = q { exactSearch = kind }
