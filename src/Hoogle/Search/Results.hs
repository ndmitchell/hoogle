
module Hoogle.Search.Results where

import Data.Char
import Data.List
import General.All

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.Search.Result


-- | Apply the PlusModule and MinusModule modes
filterResults :: Query -> [Result] -> [Result]
filterResults q xs = xs {- TODO: if null actions then xs
                     else filter (f base actions . modName . itemMod . itemResult) xs -}
    where
        actions = filter isModule $ scope q
        
        isModule (PlusModule  _) = True
        isModule (MinusModule _) = True
        isModule _ = False
        
        base = case head actions of
                    PlusModule _ -> False
                    _ -> True
        
        f z [] y = z
        f z (PlusModule  x:xs) y | doesMatch x y = f True  xs y
        f z (MinusModule x:xs) y | doesMatch x y = f False xs y
        f z (x:xs) y = f z xs y

        -- match if x is further up the tree than y
        doesMatch [] y = True
        doesMatch (x:xs) (y:ys) = x == y && doesMatch xs ys
        doesMatch _ _ = False


-- | Put the results in the correct order, by score
orderResults :: [Result] -> [Result]
orderResults = map snd . sortBy (compare `on` fst) . map (\x -> (f x, x))
    where
        f r = (resultScore r
              ,maybe 0 (length . moduleName . fst) $ resultModPkg r
              ,map toLower $ entryName $ resultEntry r)

