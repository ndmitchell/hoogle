{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor, ViewPatterns #-}
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
import General.Str
import General.IString
import Prelude
import Data.Aeson.Types
import qualified Data.Text as T
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
    = IPackage PkgName
    | IModule ModName
    | IName Str
    | ISignature (Sig IString)
    | IAlias Str [IString] (Sig IString)
    | IInstance (Sig IString)
      deriving (Show,Eq,Ord,Typeable,Data)

instance NFData Item where
    rnf (IPackage x) = rnf x
    rnf (IModule x) = rnf x
    rnf (IName x) = x `seq` ()
    rnf (ISignature x) = rnf x
    rnf (IAlias a b c) = rnf (a,b,c)
    rnf (IInstance a) = rnf a

itemName :: Item -> Maybe Str
itemName (IPackage x) = Just x
itemName (IModule x) = Just x
itemName (IName x) = Just x
itemName (ISignature _) = Nothing
itemName (IAlias x _ _) = Just x
itemName (IInstance _) = Nothing


---------------------------------------------------------------------
-- DATABASE

newtype TargetId = TargetId Word32 deriving (Eq,Ord,Storable,NFData,Ix,Typeable)

instance Show TargetId where
    show (TargetId x) = showHex x ""

-- | A location of documentation.
data Target = Target
    {targetURL :: URL -- ^ URL where this thing is located
    ,targetPackage :: Maybe (PkgName, URL) -- ^ Name and URL of the package it is in (Nothing if it is a package)
    ,targetModule :: Maybe (ModName, URL) -- ^ Name and URL of the module it is in (Nothing if it is a package or module)
    ,targetType :: String -- ^ One of package, module or empty string
    ,targetItem :: String -- ^ HTML span of the item, using <0> for the name and <1> onwards for arguments
    ,targetDocs :: String -- ^ HTML documentation to show, a sequence of block level elements
    } deriving (Show,Eq,Ord)

instance NFData Target where
    rnf (Target a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

instance ToJSON Target where
    toJSON (Target a b c d e f) = object [
      ("url" :: T.Text, toJSON a),
      ("package" :: T.Text, maybeNamedURL b),
      ("module" :: T.Text, maybeNamedURL c),
      ("type" :: T.Text, toJSON d),
      ("item" :: T.Text, toJSON e),
      ("docs" :: T.Text, toJSON f)
      ]
      where
        maybeNamedURL m = maybe emptyObject namedURL m
        namedURL (name, url) = object [("name" :: T.Text, toJSON $ strUnpack name), ("url" :: T.Text, toJSON url)]

instance FromJSON Target where
  parseJSON = withObject "target" $ \o ->
    Target <$> o .: ("url" :: T.Text)
           <*> maybeNamedURL ("package" :: T.Text) o
           <*> maybeNamedURL ("module" :: T.Text) o
           <*> o .: ("type" :: T.Text)
           <*> o .: ("item" :: T.Text)
           <*> o .: ("docs" :: T.Text)
    where maybeNamedURL na p = do
              maybeObj <- p .:? na
              case maybeObj of Nothing -> return Nothing
                               Just q -> do
                                  name <- q .: ("name" :: T.Text)
                                  url  <- q .: ("url" :: T.Text)
                                  return $ Just (strPack name, url)

targetExpandURL :: Target -> Target
targetExpandURL t@Target{..} = t{targetURL = url, targetModule = second (const mod) <$> targetModule}
    where
        pkg = maybe "" snd targetPackage
        mod = maybe pkg (plus pkg . snd) targetModule
        url = plus mod targetURL

        plus a b | b == "" = ""
                 | ':':_ <- dropWhile isAsciiLower b = b -- match http: etc
                 | otherwise = a ++ b


splitIPackage, splitIModule :: [(a, Item)] -> [(Str, [(a, Item)])]
splitIPackage = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitIModule = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe Str) -> [a] -> [(Str, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe mempty $ f x, x:a), b)


---------------------------------------------------------------------
-- HSE CONVERSION

hseToSig :: Type a -> Sig String
hseToSig = tyForall
    where
        -- forall at the top is different
        tyForall (TyParen _ x) = tyForall x
        tyForall (TyForall _ _ c t) | Sig cs ts <- tyForall t =
            Sig (maybe [] (concatMap ctx . fromContext) c ++ cs) ts
        tyForall x = Sig [] $ tyFun x

        tyFun (TyParen _ x) = tyFun x
        tyFun (TyFun _ a b) = ty a : tyFun b
        tyFun x = [ty x]

        ty (TyForall _ _ _ x) = TCon "\\/" [ty x]
        ty x@TyFun{} = TCon "->" $ tyFun x
        ty (TyTuple an box ts) = TCon (fromQName $ Special an $ TupleCon an box $ length ts - 1) (map ty ts)
        ty (TyList _ x) = TCon "[]" [ty x]
        ty (TyParArray _ x) = TCon "[::]" [ty x]
        ty (TyApp _ x y) = case ty x of
            TCon a b -> TCon a (b ++ [ty y])
            TVar a b -> TVar a (b ++ [ty y])
        ty (TyVar _ x) = TVar (fromName x) []
        ty (TyCon _ x) = TCon (fromQName x) []
#if !defined(MIN_VERSION_haskell_src_exts) || MIN_VERSION_haskell_src_exts(1,20,0)
        ty (TyInfix an a (UnpromotedName _ b) c) = ty $ let ap = TyApp an in TyCon an b `ap` a `ap` c
#else
        ty (TyInfix an a b c) = ty $ let ap = TyApp an in TyCon an b `ap` a `ap` c
#endif
        ty (TyKind _ x _) = ty x
        ty (TyBang _ _ _ x) = ty x
        ty (TyParen _ x) = ty x
        ty _ = TVar "_" []

        ctx (ParenA _ x) = ctx x
        ctx (InfixA an a con b) = ctx $ ClassA an con [a,b]
        ctx (ClassA _ con (TyVar _ var:_)) = [Ctx (fromQName con) (fromName var)]
        ctx _ = []


hseToItem :: Decl a -> [Item]
hseToItem (TypeSig _ names ty) = ISignature (toIString . strPack <$> hseToSig ty) : map (IName . strPack . fromName) names
hseToItem (TypeDecl _ (fromDeclHead -> (name, bind)) rhs) = [IAlias (strPack $ fromName name) (map (toIString . strPack . fromName . fromTyVarBind) bind) (toIString . strPack <$> hseToSig rhs)]
hseToItem (InstDecl an _ (fromIParen -> IRule _ _ ctx (fromInstHead -> (name, args))) _) = [IInstance $ fmap (toIString . strPack) $ hseToSig $ TyForall an Nothing ctx $ applyType (TyCon an name) args]
hseToItem x = map (IName . strPack) $ declNames x
