{-# LANGUAGE ViewPatterns, PatternGuards, GeneralizedNewtypeDeriving, DeriveDataTypeable, OverloadedStrings #-}

-- | Types used to generate the input.
module Input.Type(
    Sig(..), Ctx(..), Ty(..)
    ) where

import Data.Data
import Control.DeepSeq


-- FIXME: Delete the Read instances
data Sig n = Sig [Ctx n] [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- list of -> types
data Ctx n = Ctx n n deriving (Show,Eq,Ord,Typeable,Data,Read) -- context, second will usually be a free variable
data Ty n = TCon n [Ty n] | TVar n [Ty n] deriving (Show,Eq,Ord,Typeable,Data,Read) -- type application, vectorised, all symbols may occur at multiple kinds

instance NFData n => NFData (Sig n) where rnf (Sig x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ctx n) where rnf (Ctx x y) = rnf x `seq` rnf y
instance NFData n => NFData (Ty  n) where
    rnf (TCon x y) = rnf x `seq` rnf y
    rnf (TVar x y) = rnf x `seq` rnf y
