-- | High level Hoogle API
module Hoogle
  ( Database,
    withDatabase,
    searchDatabase,
    defaultDatabaseLocation,
    Target (..),
    URL,
    hoogle,
    targetInfo,
    targetResultDisplay,
  )
where

import Action.CmdLine
import Action.Generate
import Action.Search
import Action.Server
import Action.Test
import Control.DeepSeq (NFData)
import Data.Char
import qualified Data.Vector.Storable as V
import General.Store
import General.Store (storeRead)
import General.Util
import Input.Item
import Network.HTTP.Client.Conduit (Response (responseBody))
import Network.HTTP.Simple
import Numeric (readHex)
import Output.Types (TypesDuplicates (TypesDuplicates), readDuplicates)
import Query

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
    Search {} -> actionSearch args
    Generate {} -> actionGenerate args
    Server {} -> actionServer args
    Test {} -> actionTest args
    Replay {} -> actionReplay args
