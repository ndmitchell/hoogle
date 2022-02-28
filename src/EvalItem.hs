{-# LANGUAGE DeriveGeneric #-}

module EvalItem where

import qualified Data.Aeson as A
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary (Word32, encode)
import System.IO (withFile, IOMode (WriteMode))

data EvalItem = EvalItem {storageId :: Int, docType :: String, docQuery :: String, queryRes :: Maybe [Word32]} deriving (Generic, Show)

instance A.ToJSON EvalItem where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON EvalItem

readEvalItems :: FilePath -> IO [EvalItem]
readEvalItems f = do
    let parse = A.decode :: BL.ByteString -> Maybe EvalItem
    maybeContent <- traverse parse . BL.lines <$> BL.readFile f
    let result = case maybeContent of
          Nothing -> error "Could not deserialize EvalItems."
          Just content -> content
    return result

writeEvalItems :: FilePath -> [EvalItem] -> IO ()
writeEvalItems f items = do
  withFile f WriteMode $ \handle -> do
      mapM_ (\item -> BL.hPutStrLn handle $ A.encode item) items

toJson :: EvalItem -> BL.ByteString
toJson = A.encode

myPrint :: A.ToJSON a => a -> IO ()
myPrint = BL.putStrLn . A.encode

evlpath = ".\\src\\Rank\\notebooks\\data_preparation\\test-tfidf-evalset.jsonl"
