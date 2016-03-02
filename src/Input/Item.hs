{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, PatternGuards, ScopedTypeVariables #-}

-- | Types used to generate the input.
module Input.Item(
    Sig(..), Ctx(..), Ty(..), prettySig,
    Item(..), itemName,
    Target(..), targetExpandURL, TargetId(..),
    splitIPackage, splitIModule,
    hseToSig, hseToItem
    ) where

import Numeric
import Control.Applicative
import Data.Tuple.Extra
import Language.Haskell.Exts
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Ix
import Data.Binary
import Foreign.Storable
import Control.DeepSeq
import Data.Data
import General.Util
import General.IString
import Prelude


---------------------------------------------------------------------
-- TYPES

data Sig n = Sig {sigCtx :: [Ctx n], sigTy :: [Ty n]} deriving (Show,Eq,Ord,Typeable,Data,Functor) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Eq,Ord,Typeable,Data,Functor) -- context, second will usually be a free variable
data Ty n = TCon n [Ty n] | TVar n [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Functor) -- type application, vectorised, all symbols may occur at multiple kinds

instance NFData n => NFData (Sig n) where rnf (Sig x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ctx n) where rnf (Ctx x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ty  n) where
    rnf (TCon x y) = rnf x `seq` rnf y
    rnf (TVar x y) = rnf x `seq` rnf y

instance Binary n => Binary (Sig n) where
    put (Sig a b) = put a >> put b
    get = liftA2 Sig get get

instance Binary n => Binary (Ctx n) where
    put (Ctx a b) = put a >> put b
    get = liftA2 Ctx get get

instance Binary n => Binary (Ty n) where
    put (TCon x y) = put (0 :: Word8) >> put x >> put y
    put (TVar x y) = put (1 :: Word8) >> put x >> put y
    get = do i :: Word8 <- get; liftA2 (case i of 0 -> TCon; 1 -> TVar) get get

prettySig :: Sig String -> String
prettySig Sig{..} =
        (if length ctx > 1 then "(" ++ ctx ++ ") => "
         else if null ctx then "" else ctx ++ " => ") ++
        intercalate " -> " (map f sigTy)
    where
        ctx = intercalate ", " [a ++ " " ++ b | Ctx a b <- sigCtx]

        f (TVar x xs) = f $ TCon x xs
        f (TCon x []) = x
        f (TCon x xs) = "(" ++ unwords (x : map f xs) ++ ")"


---------------------------------------------------------------------
-- ITEMS

data Item
    = IPackage String
    | IModule String
    | IName String -- class or newtype
    | ISignature String (Sig IString)
    | IAlias String [IString] (Sig IString)
    | IInstance (Sig IString)
      deriving (Show,Eq,Ord,Typeable,Data)

instance NFData Item where
    rnf (IPackage x) = rnf x
    rnf (IModule x) = rnf x
    rnf (IName x) = rnf x
    rnf (ISignature a b) = rnf (a,b)
    rnf (IAlias a b c) = rnf (a,b,c)
    rnf (IInstance a) = rnf a

itemName :: Item -> Maybe String
itemName (IPackage x) = Just x
itemName (IModule x) = Just x
itemName (IName x) = Just x
itemName (ISignature x _) = Just x
itemName (IAlias x _ _) = Just x
itemName (IInstance _) = Nothing


---------------------------------------------------------------------
-- DATABASE

newtype TargetId = TargetId Word32 deriving (Eq,Ord,Storable,NFData,Ix,Typeable)

instance Show TargetId where
    show (TargetId x) = showHex x ""

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

targetExpandURL :: Target -> Target
targetExpandURL t@Target{..} = t{targetURL = url, targetModule = second (const mod) <$> targetModule}
    where
        pkg = maybe "" snd targetPackage
        mod = maybe pkg (plus pkg . snd) targetModule
        url = plus mod targetURL

        plus a b | b == "" = ""
                 | ':':_ <- dropWhile isAsciiLower b = b -- match http: etc
                 | otherwise = a ++ b


splitIPackage, splitIModule :: [(a, Item)] -> [(String, [(a, Item)])]
splitIPackage = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitIModule = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe String) -> [a] -> [(String, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe "" $ f x, x:a), b)


---------------------------------------------------------------------
-- HSE CONVERSION

hseToSig :: Type -> Sig String
hseToSig = tyForall
    where
        -- forall at the top is different
        tyForall (TyParen x) = tyForall x
        tyForall (TyForall _ c t) | Sig cs ts <- tyForall t = Sig (concatMap ctx c ++ cs) ts
        tyForall x = Sig [] $ tyFun x

        tyFun (TyParen x) = tyFun x
        tyFun (TyFun a b) = ty a : tyFun b
        tyFun x = [ty x]

        ty (TyForall _ _ x) = TCon "\\/" [ty x]
        ty x@TyFun{} = TCon "->" $ tyFun x
        ty (TyTuple box ts) = TCon (fromQName $ Special $ TupleCon box $ length ts - 1) (map ty ts)
        ty (TyList x) = TCon "[]" [ty x]
        ty (TyParArray x) = TCon "[::]" [ty x]
        ty (TyApp x y) = case ty x of
            TCon a b -> TCon a (b ++ [ty y])
            TVar a b -> TVar a (b ++ [ty y])
        ty (TyVar x) = TVar (fromName x) []
        ty (TyCon x) = TCon (fromQName x) []
        ty (TyInfix a b c) = ty $ TyCon b `TyApp` a `TyApp` c
        ty (TyKind x _) = ty x
        ty (TyBang _ x) = ty x
        ty (TyParen x) = ty x
        ty _ = TVar "_" []

        ctx (ParenA x) = ctx x
        ctx (InfixA a con b) = ctx $ ClassA con [a,b]
        ctx (ClassA con (TyVar var:_)) = [Ctx (fromQName con) (fromName var)]
        ctx _ = []


hseToItem :: Decl -> Maybe Item
hseToItem (TypeSig _ [name] ty) = Just $ ISignature (fromName name) (toIString <$> hseToSig ty)
hseToItem (TypeDecl _ name bind rhs) = Just $ IAlias (fromName name) (map (toIString . fromName . fromTyVarBind) bind) (toIString <$> hseToSig rhs)
hseToItem (InstDecl _ _ _ ctx name args _) = Just $ IInstance $ fmap toIString $ hseToSig $ TyForall Nothing ctx $ applyType (TyCon name) args
hseToItem x | [x] <- declNames x = Just $ IName x
hseToItem x = Nothing
