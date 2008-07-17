{-|
    Deal with variable bindings/alpha renaming in searches
-}

module Hoogle.DataBase.TypeSearch.Binding(
    Binding, alphaFlatten, bindCompose, bindMerge, bindCost, reverseBinding
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
alphaFlatten :: TypeSimp -> (Binding,TypeSimp)
alphaFlatten (TypeSimp a b) = (Binding $ sort bind, TypeSimp a2 $ normaliseType b2)
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
bindCompose a b = traceShow (a,b,res) res
    where res = Binding [(a1,b2) | (a1,a2) <- fromBinding a, (b1,b2) <- fromBinding b, a2 == b1]


bindMerge :: [Binding] -> Binding
bindMerge = Binding . concatMap fromBinding


bindCost :: Binding -> [Cost]
bindCost (Binding bind) = f id bind ++ f CostReverse (map swap bind)
    where f op xs = [newCost $ op $ CostVar a nb | (a,b) <- groupFsts $ sortFst xs, let nb = nub b, length nb > 1]
