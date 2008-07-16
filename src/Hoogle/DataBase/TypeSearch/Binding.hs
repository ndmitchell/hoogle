{-|
    Deal with variable bindings/alpha renaming in searches
-}

module Hoogle.DataBase.TypeSearch.Binding(
    Binding, alphaFlatten, bindCompose
    ) where

import Hoogle.TypeSig.All
import Data.Generics.Uniplate
import General.Code
import Control.Monad.State hiding (put,get)
import qualified Control.Monad.State as S


type Binding = [(String,String)]


-- normalise the letters in a type, so that:
-- each variable is distinct
-- the context is in order
-- all context relates to free variables
-- binding is original |-> new
alphaFlatten :: TypeSimp -> (Binding,TypeSimp)
alphaFlatten (TypeSimp a b) = (sort bind, TypeSimp a2 $ normaliseType b2)
    where
        a2 = nub $ sort $ concatMap g a
        (b2,(bind,_)) = runState (transformM f b) ([], map (:[]) ['a'..])

        f (TVar x) = do
            (bind,v:vs) <- S.get
            S.put ((x,v):bind,vs)
            return $ TVar v
        f x = return x

        g (cls,v) = [(cls,b) | (a,b) <- bind, a == v]


bindCompose :: Binding -> Binding -> Binding
bindCompose a b = a
