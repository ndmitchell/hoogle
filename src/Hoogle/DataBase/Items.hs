
module Hoogle.DataBase.Items(Items, createItems, getItemFromId) where

import Hoogle.Item.All
import Data.Array
import Data.Binary.Defer


data Items = Items (Array Int (Defer Item))


-- extract the items that may be used in future, and assign them Id's
createItems :: [Item] -> ([Item],Items)
createItems items = (items2, Items $ listArray (0,length res - 1) (map Defer res))
    where
        (items2,res) = f 0 items
    
        -- number to use next, list of items = (ret,use)
        f :: Int -> [Item] -> ([Item],[Item])
        f i [] = ([],[])
        f i (x:xs) = (x{itemId=if wanted x then i else 0}:a, b)
            where (a,b) = f (if wanted x then i+1 else i) xs

        wanted = not . null . itemName


getItemFromId :: Items -> Int -> Item
getItemFromId (Items x) i = fromDefer (x ! i)


