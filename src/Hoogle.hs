{-# LANGUAGE DeriveDataTypeable #-}

-- The plan, over time, is to make this module simply reexport things, integrating the wrapper
-- layer back into Hoogle proper

-- | The Hoogle API.
module Hoogle(
    -- * Utility types
    module Hoogle.Type.TagStr,
    H.ParseError(..), H.emptyParseError,
    URL,
    Language(..),
    -- * Database
    Database, loadDatabase, saveDatabase, createDatabase, showDatabase,
    -- * Query
    Query, parseQuery, H.renderQuery, H.isBlankQuery,
    queryDatabases, querySuggestions, queryCompletions,
    -- * Score
    Score, H.scoring,
    -- * Search
    Result(..), searchAll, searchRange
    ) where

import Data.Binary.Defer.Index
import Data.Data
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


-- * Utility types

data Language = Haskell
    deriving (Enum,Read,Show,Eq,Ord,Bounded,Data,Typeable)

-- * Database

newtype Database = Database [H.DataBase]

toDataBase (Database x) = H.combineDataBase x
fromDataBase x = Database [x]

instance Monoid Database where
    mempty = Database []
    mappend (Database xs) (Database ys) = Database $ xs ++ ys

instance Show Database where
    show = show . toDataBase


loadDatabase :: FilePath -> IO Database
loadDatabase = fmap fromDataBase . H.loadDataBase


showDatabase :: Database -> Maybe [String] -> String
showDatabase x sects = concatMap (`H.showDataBase` toDataBase x) $ fromMaybe [""] sects


-- | From a textbase lines we have currently
createDatabase :: Language -> [Database] -> String -> ([H.ParseError], Database)
createDatabase _ dbs src = (err, fromDataBase $ H.createDataBase xs res)
    where
        (err,res) = H.parseInputHaskell src
        xs = concat [x | Database x <- dbs]


saveDatabase :: FilePath -> Database -> IO ()
saveDatabase file x = do
    performGC
    H.saveDataBase file $ toDataBase x


-- Hoogle.Query
parseQuery :: Language -> String -> Either H.ParseError Query
parseQuery _ = H.parseQuery

queryDatabases :: Query -> [String]
queryDatabases x = if null ps then ["default"] else ps
    where ps = [p | H.PlusPackage p <- H.scope x]

querySuggestions :: Database -> Query -> Maybe TagStr
querySuggestions (Database dbs) q = H.suggestQuery dbs q

queryCompletions :: Database -> String -> [String]
queryCompletions x = H.completions (toDataBase x)


-- Hoogle.Search

data Result = Result
    {package :: Maybe (URL, String)
    ,modul :: Maybe (URL, String)
    ,self :: (URL, TagStr)
    ,docs :: TagStr
    }

toResult :: H.Result -> (Score,Result)
toResult r@(H.Result entry view score) = (score, Result package modul self docs)
    where
        ent = fromLink entry
        (modu,text,_) = H.renderResult r

        package = let p = fromLink $ H.entryPackage ent in Just (H.packageURL p, H.packageName p)
        modul = fmap (\x -> (H.moduleURL x, show x)) modu
        self = (H.entryURL ent, text)
        docs = H.renderDocumentation $ H.entryDocs ent


searchAll :: Database -> Query -> [(Score,Result)]
searchAll (Database xs) q = map toResult $ H.searchAll xs q


-- | A pair of bounds. These bounds are the lowest and highest indices in the array, in that order.
--   For example, the first 10 elements are (0,9) and the next 10 are (10,19)
searchRange :: (Int,Int) -> Database -> Query -> [(Score,Result)]
searchRange (a,b) (Database xs) q = map toResult $ H.searchRange (a,b) xs q
