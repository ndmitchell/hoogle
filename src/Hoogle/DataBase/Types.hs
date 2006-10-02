
module Hoogle.DataBase.Types(saveTypes, searchTypes) where

import System.IO

import General.All
import Hoogle.Common.All
import Hoogle.TypeSig.All

{-
FORMAT

n = number of distinct types
then n*type-item

type-item =
    arity = int
    type structure of each of the components

    count = int
    count*{id,int*arity} -- the permutations

If two types are equal up to alpha renaming and parameter reordering
then they are merged into one type value.
-}




saveTypes :: Handle -> [Item] -> IO [Response]
saveTypes _ _ = return []



searchTypes :: Handle -> TypeSig -> IO [Result]
searchTypes _ _ = return []
