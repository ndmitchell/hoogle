{-# LANGUAGE ViewPatterns, PatternGuards, GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}

-- | Types used to generate the input.
module Input.Item(
    Target(..), Item(..), Sig(..), Ctx(..), Ty(..),
    isIPackage, isIModule, splitIPackage, splitIModule,
    URL,
    Id(..)
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


---------------------------------------------------------------------
-- DATABASE

type URL = String
newtype Id = Id Word32 deriving (Eq,Ord,Storable,NFData,Ix)

instance Show Id where
    show (Id x) = showHex x ""

instance Read Id where
    readsPrec _ = map (first Id) . readHex

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


-- FIXME: Delete the Read instances
data Sig n = Sig [Ctx n] [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Eq,Ord,Typeable,Data,Read) -- context, second will usually be a free variable
data Ty n = Ty n [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- type application, vectorised, all symbols may occur at multiple kinds

data Item
    = IDecl {fromIDecl :: Decl}
    | IKeyword {fromIKeyword :: String}
    | IPackage {fromIPackage :: String}
    | IModule {fromIModule :: String}
      deriving (Show,Eq,Ord,Typeable,Data)

instance NFData Item where
    rnf (IDecl x) = rnf $ show x
    rnf (IKeyword x) = rnf x
    rnf (IPackage x) = rnf x
    rnf (IModule x) = rnf x

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False

splitIPackage, splitIModule :: [(a, Item)] -> [(String, [(a, Item)])]
splitIPackage = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitIModule = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe String) -> [a] -> [(String, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe "" $ f x, x:a), b)
