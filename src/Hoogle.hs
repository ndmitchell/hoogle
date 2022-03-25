-- | High level Hoogle API
module Hoogle where

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
import Network.HTTP.Client.Conduit (Response (responseBody), Request (requestBody))
import Network.HTTP.Simple
import Query
import Document
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified EvalItem as E (readEvalItems, EvalItem (docQuery, queryRes, docType), writeEvalItems, toJson)
import EvalItem (writeEvalItems, EvalItem (queryRes))
import System.IO (withFile, IOMode (WriteMode, AppendMode))
import System.Directory (doesFileExist)
import Output.Items
import Network.HTTP.Conduit (simpleHttp)
import Data.List
import Data.Aeson (decode)
import Data.Binary (Word32)
import Data.Maybe (fromMaybe)

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

-- | Search a database, given a query string, produces a list of tuples.
searchDatabase' :: StoreRead -> String -> [(TargetId, Target)]
searchDatabase' store query = snd $ searchTargetsWithIds store $ parseQuery query

searchTargets :: String -> IO [Target]
searchTargets q = do
  database <- defaultDatabaseLocation
  res <- withSearch database $ \store -> do
    return $ searchDatabase' store q
  return $ map snd res

searchDocs :: String -> IO [Document]
searchDocs q = do
  database <- defaultDatabaseLocation
  res <- withSearch database $ \store -> do
    return $ searchDatabase' store q
  return $ map (\(id, t) -> toDocument (id, t)) res

putSearchDocs  :: String -> IO ()
putSearchDocs q = do
  res <- searchDocs q
  mapM_ (LBS.putStrLn . toJson) res

runEvaluation :: FilePath -> IO ()
runEvaluation f = do
  let destination = ".\\missing-eval-result-test.jsonl"
  destExists <- doesFileExist destination
  startAt <- if destExists then length . lines <$> readFile destination else return 0
  print startAt
  database <- defaultDatabaseLocation
  withSearch database $ \store -> do
    items <- E.readEvalItems f
    let enumeratedItems = drop startAt $ zip [1..] items
    let getTargetIds q = map (\(TargetId id, _) -> id) $ searchDatabase' store q
    let mode = if destExists then AppendMode else WriteMode
    withFile destination mode $ \handle ->
      mapM_ (\(n, i) -> do
        print $ "start item n" ++ show n
        let res = getTargetIds $ E.docType i
        LBS.hPutStrLn handle $ E.toJson $ i { queryRes = Just res }
        ) enumeratedItems

evlpath = ".\\src\\Rank\\notebooks\\data_preparation\\missing-tfidf-evalset.jsonl"


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
