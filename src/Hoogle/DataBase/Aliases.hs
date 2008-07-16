
module Hoogle.DataBase.Aliases(
    Aliases, createAliases, followAliases
    ) where

import Hoogle.TextBase.All
import Hoogle.TypeSig.All
import qualified Data.Binary.Defer.Map as Map
import Data.Binary.Defer
import Data.Generics.Uniplate
import General.Code


newtype Aliases = Aliases (Map.Map String [Alias])

instance BinaryDefer Aliases where
    put (Aliases a) = put a
    get = get1 Aliases

instance Show Aliases where
    show (Aliases mp) = unlines [ unwords $ "type" : s : vs ++ ["=", show t]
                                | (s,as) <- Map.toList mp, Alias vs t <- as]


data Alias = Alias
    {args :: [String] -- the free variables
    ,rhs :: Type -- the resulting type
    }

instance BinaryDefer Alias where
    put (Alias a b) = put2 a b
    get = get2 Alias


createAliases :: [TextItem] -> Aliases
createAliases ti = Aliases $ filterRecursive $ fromListMany
    [ (name, Alias [v | TVar v <- args] rhs)
    | ItemAlias (TypeSig _ lhs) (TypeSig _ rhs) <- ti
    , let (TLit name, args) = fromTApp lhs]


-- filter out the aliases which expand back to themselves
-- i.e. template-haskell has "type Doc = PprM Doc"
-- probably the result of unqualifying names
filterRecursive :: Map.Map String [Alias] -> Map.Map String [Alias]
filterRecursive mp = Map.filterWithKey f mp
    where
        f name _ = g name [] [name]

        g evil done [] = True
        g evil done (t:odo)
                | t `elem` done = g evil done odo
                | evil `elem` next = False
                | otherwise = g evil (t:done) (next++odo)
            where
                next = [x | a <- Map.findWithDefault [] t mp,  TLit x <- universe $ rhs a]



-- follow an alias at this point
followAliases :: Aliases -> Type -> [(String,Type)]
followAliases (Aliases mp) t =
    [ (x, gen $ followAlias xs a)
    | (TApp (TLit x) xs, gen) <- contexts $ insertTApp t
    , a@(Alias vs rhs) <- Map.findWithDefault [] x mp
    , length vs == length xs ]


followAlias :: [Type] -> Alias -> Type
followAlias ts (Alias vs rhs) = transform f rhs
    where
        rep = zip vs ts
        f (TVar v) = fromJust $ lookup v rep
        f x = x

