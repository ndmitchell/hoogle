{-# LANGUAGE DeriveGeneric #-}

module Corpus.Document where

import Data.Aeson
  ( ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
  )
import Data.Binary (Word32)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (toLower)
import GHC.Generics (Generic)
import General.Util (unHTML)
import Input.Item (Target (targetDocs), TargetId (..))
import System.IO (IOMode (WriteMode), withFile)

data Document = Document {id :: Word32, content :: String} deriving (Generic)

instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

toDocument :: (TargetId, Target) -> Document
toDocument (TargetId id, t) = Document id normDocs
  where
    normDocs = normalizedDocs t

normalizedDocs :: Target -> String
normalizedDocs = normalize . targetDocs

normalize :: String -> String
normalize = stringToLower . concatMap (\l -> trimConsSpace l ++ " ") . lines . unHTML
  where
    trimConsSpace = unwords . words
    stringToLower = map toLower