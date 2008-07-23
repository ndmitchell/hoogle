{-|
    Deal with variable bindings/alpha renaming in searches
-}

module Hoogle.DataBase.TypeSearch.Binding(
    Binding, fromBinding, alphaFlatten, reverseBinding
    ) where

import Hoogle.TypeSig.All
import Hoogle.DataBase.TypeSearch.Cost
import Data.Generics.Uniplate
import Data.Binary.Defer
import General.Code
import Control.Monad.State hiding (put,get)
import qualified Control.Monad.State as S


newtype Binding = Binding {fromBinding :: [(String,String)]}
                  deriving (Eq,Ord)


instance BinaryDefer Binding where
    put (Binding x) = put1 x
    get = get1 Binding


instance Show Binding where
    show (Binding xs) = "{" ++ concat (intersperse "," $ map f xs) ++ "}"
        where f (a,b) = a ++ "=" ++ b


newBinding :: Binding
newBinding = Binding []


reverseBinding :: Binding -> Binding
reverseBinding = Binding . map swap . fromBinding


-- normalise the letters in a type, so that:
-- each variable is distinct
-- the context is in order
-- all context relates to free variables
-- binding is original |-> new
alphaFlatten :: Type -> (Binding,Type)
alphaFlatten t = (Binding $ sort bind, normaliseType t2)
    where
        (t2,(bind,_)) = runState (transformM f t) ([], map (:[]) ['a'..])

        f (TVar x) = do
            (bind,v:vs) <- S.get
            S.put ((x,v):bind,vs)
            return $ TVar v
        f x = return x


-- optimise a unique binding by deleting simple renames
uniqueBinding :: Binding -> Binding
uniqueBinding (Binding bs) = Binding [(a,b) | (a,b) <- bs, a /= b]
