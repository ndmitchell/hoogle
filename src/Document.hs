{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Document where

import qualified Data.Aeson as A
import Data.Binary (Word32)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (toLower)
import Data.List.Extra (breakOn, intercalate, splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Text as DT (pack, strip, unpack)
import GHC.Generics (Generic)
import General.Util (unHTML)
import Input.Item (Target (targetDocs, targetItem, targetPackage), TargetId (..), unHTMLTarget)
import System.IO (IOMode (WriteMode), withFile)

type FunctionName = String

type FunctionType = String

data Document = Document
  { docId :: Word32,
    docContent :: String,
    docItem :: String,
    docType :: String,
    docPackage :: String
  }
  deriving (Show, Generic)

instance A.ToJSON Document where
  toEncoding = A.genericToEncoding A.defaultOptions

toDocument :: (TargetId, Target) -> Document
toDocument (TargetId id, t) = Document id docContent' docItem' docType' package
  where
    unHTMLedTarget = unHTMLTarget t
    docContent' = normalizedDocs unHTMLedTarget
    docItem' = strip $ fromMaybe "" $ getTypeSig $ targetItem unHTMLedTarget
    docType' = strip $ fromMaybe "" $ getTypeOfTypeSig $ targetItem unHTMLedTarget
    package = maybe "" fst (targetPackage unHTMLedTarget)

toJson :: Document -> LBS.ByteString
toJson = A.encode

-- cheap but enough to check whether targetItem is a type signature
getTypeSig :: String -> Maybe String
getTypeSig s = if isTypeSignature s then Just s else Nothing
  where
    isTypeSignature s = case splitOn "::" s of
      [name, typeString] -> (length $ words name) == 1
      _ -> False

-- breakOn is used to keep "::" to enforce hoogle type search
getTypeOfTypeSig :: String -> Maybe String
getTypeOfTypeSig s = snd . breakOn "::" <$> getTypeSig s

strip :: String -> String
strip = (DT.unpack . DT.strip . DT.pack)

normalizedDocs :: Target -> String
normalizedDocs = normalize . targetDocs

normalize :: String -> String
normalize = stringToLower . concatMap (\l -> trimConsSpace l ++ " ") . lines
  where
    trimConsSpace = unwords . words
    stringToLower = map toLower