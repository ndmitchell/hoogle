{-# LANGUAGE ViewPatterns, PatternGuards, GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}

-- | Types used to generate the input.
module Input.Item(
    Target(..), Item(..), Sig(..), Ctx(..), Ty(..),
    itemName,
    splitIPackage, splitIModule,
    URL,
    TargetId(..)
    ) where

import Numeric
import Data.Tuple.Extra
import Language.Haskell.Exts
import Data.List.Extra
import Data.Maybe
import Data.Ix
import Foreign.Storable
import Data.Word
import Control.DeepSeq
import Data.Data
import Input.Type
import General.Util


---------------------------------------------------------------------
-- DATABASE

type URL = String
newtype TargetId = TargetId Word32 deriving (Eq,Ord,Storable,NFData,Ix)

instance Show TargetId where
    show (TargetId x) = showHex x ""

instance Read TargetId where
    readsPrec _ = map (first TargetId) . readHex

data Target = Target
    {targetURL :: URL -- URL where this thing is located
    ,targetPackage :: Maybe (String, URL) -- name and URL of the package it is in (Nothing if it is a package)
    ,targetModule :: Maybe (String, URL) -- name and URL of the module it is in (Nothing if it is a package or module)
    ,targetType :: String -- one of package, module or empty string
    ,targetItem :: String -- HTML span of the item, using <0> for the name and <1> onwards for arguments
    ,targetDocs :: String -- HTML documentation to show, a sequence of block level elements
    } deriving (Show,Eq,Ord)

instance NFData Target where
    rnf (Target a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f


data Item
    = IDecl Decl
    | IPackage String
    | IModule String
      deriving (Show,Eq,Ord,Typeable,Data)

instance NFData Item where
    rnf (IDecl x) = rnf $ show x
    rnf (IPackage x) = rnf x
    rnf (IModule x) = rnf x

itemName :: Item -> Maybe String
itemName (IDecl x) = listToMaybe $ declNames x
itemName (IPackage x) = Just x
itemName (IModule x) = Just x


splitIPackage, splitIModule :: [(a, Item)] -> [(String, [(a, Item)])]
splitIPackage = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitIModule = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe String) -> [a] -> [(String, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe "" $ f x, x:a), b)
