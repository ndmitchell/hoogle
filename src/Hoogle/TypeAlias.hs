{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{-|
    Deal with type aliases, i.e. type String = [Char]
-}

module Hoogle.TypeAlias(
    AliasTable,
    buildAlias,
    lookupAlias,
    showAliasTable
    ) where

import Hoogle.TypeSig

import qualified Data.Map as Map
import Data.Maybe


-- | Alias is a string, being the name of the constructor
--   the number of arguments it takes, and the resulting type
--   TNum is used for replacement positions
data AliasTable = AliasTable (Map.Map String (Int, Type))
                  deriving Show


-- | Given a list of aliases, build an alias table
--   All members of Item must be TypeAlias
buildAlias :: [Item] -> AliasTable
buildAlias xs = {- AliasTable $ Map.fromList $ -} fixpAlias $ map f xs
    where
        f (TypeAlias name args ([], typ)) =
            (name, (length args, mapUnbound (g (zip args [0..])) typ))

        g lst name = case lookup name lst of
                        Just n -> TNum n
                        Nothing -> TVar name


-- yay for circular programming
-- boo for possible non-termination
fixpAlias :: [(String, (Int, Type))] -> AliasTable
fixpAlias x = map2
    where
        map2 = AliasTable $ Map.map (\(a,b) -> (a, lookupAlias map2 b)) map1
        map1 = Map.fromList x


-- | Given a type, follow all aliases to find the ultimate one
lookupAlias :: AliasTable -> Type -> Type
lookupAlias (AliasTable table) (TLit x) = typ
    where (0, typ) = Map.findWithDefault (0, TLit x) x table
    
lookupAlias a@(AliasTable table) (TList (TLit x:xs)) = if isJust res then some else none
    where
        res = Map.lookup x table
        none = TList (TLit x : map (lookupAlias a) xs)
        Just (n, typ) = res
        some = if n == length xs
               then mapNumber (xs !!) typ
               else error "lookupAlias: mismatch"

lookupAlias _ x = x

showAliasTable :: AliasTable -> String
showAliasTable (AliasTable x) = unlines $ map showAlias (Map.toList x)

showAlias (name, (args, typ)) = "type " ++ name ++ "[" ++ show args ++ "] = " ++ show typ
