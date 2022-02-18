module Dump where

import Action.Search
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Document
import Hoogle
import Input.Item (TargetId (TargetId))
import Numeric
import Output.Items
import System.IO
import General.Store (StoreRead)
import GHC.IO.Encoding

exportDatabaseAsJsonl :: IO ()
exportDatabaseAsJsonl = dump database "test.jsonl" asJson

dump :: (FilePath -> (Document -> LBS.ByteString) -> IO ()) -> FilePath -> (Document -> LBS.ByteString) -> IO ()
dump action target format = action target format

asJson :: Document -> LBS.ByteString
asJson = toJson

dumpDatabase :: Maybe Int -> FilePath -> (Document -> LBS.ByteString) -> IO ()
dumpDatabase n target f = do
  setLocaleEncoding utf8
  database <- defaultDatabaseLocation
  withSearch database $ \store -> do
    let docs = map toDocument $ filter (not . null . targetDocs . snd) $ listItemsWithIds store
    let docsToDump = case n of
          Nothing -> docs
          Just i -> take i docs
    withFile target WriteMode $ \handle -> do
      mapM_ (\doc -> LBS.hPutStrLn handle $ f doc) docsToDump

database :: FilePath -> (Document -> LBS.ByteString) -> IO ()
database target f = dumpDatabase Nothing target f

partialDatabase :: Int -> FilePath -> (Document -> LBS.ByteString) -> IO ()
partialDatabase n target f = dumpDatabase (Just n) target f