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
import Data.Binary (Word32)
import Data.Char
import Data.List (concatMap, intercalate)
import Data.Maybe (listToMaybe)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import General.Store
import General.Util
import Input.Item
import Network.HTTP.Client.Conduit (Response (responseBody))
import Network.HTTP.Simple
import Numeric (readHex)
import Output.Items (listItemsWithIds, lookupItem)
import Query
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

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

dumpDatabaseAsCsvDefault :: IO ()
dumpDatabaseAsCsvDefault = dumpDatabaseAsCsv "small.dump.csv"

dumpDatabaseAsCsv :: FilePath -> IO ()
dumpDatabaseAsCsv f = do
  database <- defaultDatabaseLocation
  withSearch database $ \store -> do
    let items = take 20 $ filter (not . null . targetDocs . snd) $ listItemsWithIds store
    withFile f WriteMode $ \handle -> do
      let surroundWithQuotes s = "\"" ++ s ++ "\"" 
      let header = surroundWithQuotes "id" ++ ";" ++ surroundWithQuotes "content"
      hPutStrLn handle header
      mapM_ (\(id, t) -> hPutStrLn handle $ surroundWithQuotes (show id) ++ ";" ++ (surroundWithQuotes (normalize $ targetDocs t))) items
    return ()
  return ()

normalize :: String -> String
normalize = stringToLower . concatMap (\l -> trimConsSpace l ++ " ") . lines . unHTML
  where
    trimConsSpace = unwords . words
    stringToLower = map toLower

maybeReadHex :: (Eq a, Num a) => String -> Maybe a
maybeReadHex s = listToMaybe $ map fst $ readHex s

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
