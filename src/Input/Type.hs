{-# LANGUAGE ViewPatterns, PatternGuards, GeneralizedNewtypeDeriving, DeriveDataTypeable, OverloadedStrings #-}

-- | Types used to generate the input.
module Input.Type(
    Sig(..), Ctx(..), Ty(..),
    toSig
    ) where

import Data.Data
import Control.DeepSeq
import Language.Haskell.Exts
import General.Util
import General.IString
import Data.Maybe


-- FIXME: Delete the Read instances
data Sig n = Sig [Ctx n] [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Eq,Ord,Typeable,Data,Read) -- context, second will usually be a free variable
data Ty n = TCon n [Ty n] | TVar n [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- type application, vectorised, all symbols may occur at multiple kinds

instance NFData n => NFData (Sig n) where rnf (Sig x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ctx n) where rnf (Ctx x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ty  n) where
    rnf (TCon x y) = rnf x `seq` rnf y
    rnf (TVar x y) = rnf x `seq` rnf y


toSig :: Type -> Sig IString
toSig = tyForall
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
