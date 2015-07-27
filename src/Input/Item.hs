{-# LANGUAGE ViewPatterns, PatternGuards, GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances, OverloadedStrings #-}

-- | Types used to generate the input.
module Input.Item(
    Sig(..), Ctx(..), Ty(..),
    Item(..), itemName,
    Target(..), TargetId(..),
    splitIPackage, splitIModule,
    hseToSig
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
import General.Util
import General.IString


---------------------------------------------------------------------
-- TYPES

-- FIXME: Delete the Read instances
data Sig n = Sig [Ctx n] [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Eq,Ord,Typeable,Data,Read) -- context, second will usually be a free variable
data Ty n = TCon n [Ty n] | TVar n [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- type application, vectorised, all symbols may occur at multiple kinds

instance NFData n => NFData (Sig n) where rnf (Sig x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ctx n) where rnf (Ctx x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ty  n) where
    rnf (TCon x y) = rnf x `seq` rnf y
    rnf (TVar x y) = rnf x `seq` rnf y


---------------------------------------------------------------------
-- ITEMS

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


---------------------------------------------------------------------
-- DATABASE

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


splitIPackage, splitIModule :: [(a, Item)] -> [(String, [(a, Item)])]
splitIPackage = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitIModule = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe String) -> [a] -> [(String, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe "" $ f x, x:a), b)


---------------------------------------------------------------------
-- HSE CONVERSION

hseToSig :: Type -> Sig IString
hseToSig = tyForall
    where
        -- forall at the top is different
        tyForall (TyParen x) = tyForall x
        tyForall (TyForall _ c t) | Sig cs ts <- tyForall t = Sig (toCtx c ++ cs) ts
        tyForall x = Sig [] $ tyFun x

        tyFun (TyParen x) = tyFun x
        tyFun (TyFun a b) = ty a : tyFun b
        tyFun x = [ty x]

        ty (TyForall _ _ x) = TCon "\\/" [ty x]
        ty x@TyFun{} = TCon "->" $ tyFun x
        ty (TyTuple box ts) = TCon (toIString $ fromQName $ Special $ TupleCon box $ length ts) (map ty ts)
        ty (TyList x) = TCon "[]" [ty x]
        ty (TyParArray x) = TCon "[::]" [ty x]
        ty (TyApp x y) = case ty x of
            TCon a b -> TCon a (b ++ [ty y])
            TVar a b -> TVar a (b ++ [ty y])
        ty (TyVar x) = TVar (toIString $ fromName x) []
        ty (TyCon x) = TCon (toIString $ fromQName x) []
        ty (TyInfix a b c) = ty $ TyCon b `TyApp` a `TyApp` c
        ty (TyKind x _) = ty x
        ty (TyBang _ x) = ty x
        ty _ = TVar "_" []

toCtx :: Context -> [Ctx IString]
toCtx = mapMaybe f
    where
        f (ParenA x) = f x
        f (InfixA a con b) = f $ ClassA con [a,b]
        f (ClassA con (TyVar var:_)) = Just $ Ctx (toIString $ fromQName con) (toIString $ fromName var)
        f _ = Nothing
