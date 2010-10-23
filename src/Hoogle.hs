{-# LANGUAGE DeriveDataTypeable #-}

-- The plan, over time, is to make this module simply reexport things, integrating the wrapper
-- layer back into Hoogle proper

-- | The Hoogle API.
module Hoogle(
    -- * Utility types
    module Data.TagStr,
    ParseError(..),
    URL,
    -- * Database
    Database, loadDatabase, saveDatabase, createDatabase, showDatabase,
    -- * Query
    H.Query, parseQuery, H.renderQuery, H.isBlankQuery,
    queryDatabases, querySuggestions, queryCompletions,
    -- * Score
    Score, scoring,
    -- * Search
    Result(..), searchAll, searchRange
    ) where

import Data.Binary.Defer.Index
import Data.Monoid
import Data.Range
import Data.Data
import Data.Maybe
import Data.TagStr
import Text.ParserCombinators.Parsec(sourceColumn, sourceLine, errorPos)

import qualified Hoogle.DataBase.All as H
import qualified Hoogle.Query.All as H
import qualified Hoogle.Search.All as H
import qualified Hoogle.Item.All as H
import qualified Hoogle.TextBase.All as H


-- * Utility types

type URL = String

-- | 1 based
data ParseError = ParseError {lineNo :: Int, columnNo :: Int, parseError :: String}
                  deriving (Ord,Eq,Data,Typeable)

instance Show ParseError where
    show (ParseError line col err) = "Parse error: " ++ err ++ ":" ++ show line ++ ":" ++ show col

toParseError x = ParseError (sourceLine $ errorPos x) (sourceColumn $ errorPos x) (show x)


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
createDatabase :: [Database] -> String -> Either ParseError Database
createDatabase dbs = either (Left . toParseError) (Right . fromDataBase . H.createDataBase xs) . H.parseTextBaseString
    where xs = concat [x | Database x <- dbs]


saveDatabase :: FilePath -> Database -> IO ()
saveDatabase file = H.saveDataBase file . toDataBase


-- Hoogle.Query

type Query = H.Query

parseQuery :: String -> Either ParseError Query
parseQuery = either (Left . toParseError) Right . H.parseQuery

queryDatabases :: Query -> [String]
queryDatabases x = if null ps then ["default"] else ps
    where ps = [p | H.PlusPackage p <- H.scope x]

querySuggestions :: Database -> Query -> Maybe TagStr
querySuggestions (Database dbs) q = H.suggestQuery dbs q

queryCompletions :: Database -> String -> [String]
queryCompletions x = H.completions (toDataBase x)


-- Hoogle.Score

newtype Score = Score [H.Score]
                deriving (Eq,Ord,Show)

instance Monoid Score where
    mempty = Score []
    mappend (Score xs) (Score ys) = Score $ xs ++ ys

-- | A list of scores where one is lower than the other, returns the score result.
--   In the 'IO' monad since it may require randomness, and it may output status messages while solving,
--   particularly if in Verbose mode.
scoring :: [(Score,Score)] -> IO String
scoring _ = error "scoring not yet implemented"
{-
    where
        -- generate initial bounds
        -- refine the bounds iteratively while still consistent
        generate = [(x,[1..5]) | x <- ['a'..'z']]
        refine :: Eq a => [(a,[b])] -> ((a -> [b]) -> [(a,[b])]) -> [(a,[b])]
        refine initial step = undefined
-}

-- Hoogle.Search

data Result = Result
    {package :: Maybe (URL, String)
    ,modul :: Maybe (URL, String)
    ,self :: (URL, TagStr)
    ,docs :: TagStr
    }

toResult :: H.Result -> (Score,Result)
toResult r@(H.Result entry view score) = (Score score, Result package modul self docs)
    where
        ent = fromLink entry
        (modu,text,_) = H.renderResult r

        package = Just (H.entryPackageURL ent, H.packageName $ fromLink $ H.entryPackage ent)
        modul = fmap (\x -> (H.entryModuleURL ent, H.showModule x)) modu
        self = (H.entryURL ent, text)
        docs = H.renderHaddock $ H.entryDocs ent


searchAll :: Database -> Query -> [(Score,Result)]
searchAll (Database xs) q = map toResult $ H.searchAll xs q


-- | A pair of bounds. These bounds are the lowest and highest indices in the array, in that order.
--   For example, the first 10 elements are (0,9) and the next 10 are (10,19)
searchRange :: (Int,Int) -> Database -> Query -> [(Score,Result)]
searchRange (a,b) (Database xs) q = map toResult $ H.searchRange (rangeStartEnd a b) xs q
