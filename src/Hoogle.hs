
-- | High level Hoogle API
module Hoogle(
    Database, withDatabase, searchDatabase, defaultDatabaseLocation,
    Target(..), URL,
    hoogle,
    targetInfo,
    targetResultDisplay
    ) where

import Control.DeepSeq (NFData)

import Query
import Input.Item
import General.Util
import General.Store

import Action.CmdLine
import Action.Generate
import Action.Search
import Action.Server
import Action.Test


-- | Database containing Hoogle search data.
newtype Database = Database StoreRead

-- | Load a database from a file.
withDatabase :: NFData a => FilePath -> (Database -> IO a) -> IO a
withDatabase file act = storeReadFile file $ act . Database

-- | The default location of a database
defaultDatabaseLocation :: IO FilePath
defaultDatabaseLocation = defaultDatabaseLang Haskell

-- | Search a database, given a query string, produces a list of results.
searchDatabase :: Database -> String -> [Target]
searchDatabase (Database db) query = snd $ search db $ parseQuery query


-- | Run a command line Hoogle operation.
hoogle :: [String] -> IO ()
hoogle args = do
    args <- getCmdLine args
    case args of
        Search{} -> actionSearch args
        Generate{} -> actionGenerate args
        Server{} -> actionServer args
        Test{} -> actionTest args
        Replay{} -> actionReplay args
