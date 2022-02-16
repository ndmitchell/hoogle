{-# LANGUAGE DeriveGeneric #-}

module Document where

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
import Input.Item (Target (targetDocs, targetItem), TargetId (..), unHTMLTarget)
import System.IO (IOMode (WriteMode), withFile)
import Data.List.Extra (splitOn)
import qualified Data.Text as DT (strip, unpack, pack)
import Data.Maybe (fromMaybe)

type FunctionName = String
type FunctionType = String
data Document = Document {id :: Word32, content :: String, functionName :: FunctionName, typeString :: FunctionType} deriving (Generic)

instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

toDocument :: (TargetId, Target) -> Document
toDocument (TargetId id, t) = Document id normDocs name typeAsString
  where
    unHTMLedTarget = unHTMLTarget t
    normDocs = normalizedDocs unHTMLedTarget
    targetItemString = targetItem unHTMLedTarget
    maybeSig = maybeTypeSignature targetItemString
    (name, typeAsString) = case maybeSig of
      Nothing -> ("", "") 
      Just (functionName, functionType) -> (functionName, functionType)

normalizedDocs :: Target -> String
normalizedDocs = normalize . targetDocs

-- | Returns the type as string if the given string is a type signature
maybeTypeSignature :: String -> Maybe (FunctionName, FunctionType)
maybeTypeSignature s = case splitOn "::" s of
  [name, typeString] -> if (length $ words name) == 1 then Just (strip name, strip typeString) else Nothing
  _ -> Nothing

strip :: String -> String
strip = (DT.unpack . DT.strip . DT.pack)

normalize :: String -> String
normalize = stringToLower . concatMap (\l -> trimConsSpace l ++ " ") . lines
  where
    trimConsSpace = unwords . words
    stringToLower = map toLower