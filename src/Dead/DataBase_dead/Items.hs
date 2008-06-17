
module Hoogle.DataBase.Items(Items, createItems, getItemFromId) where

import Hoogle.Item.All
import Data.Array
import Data.Binary.Defer
import Hoogle.DataBase.BinaryDefer


data Items = Items (Array Int (Defer Item))
             deriving Show

instance BinaryDefer Items where
    bothDefer = defer [\ ~(Items a) -> unit Items << a]


-- extract the items that may be used in future, and assign them Id's
createItems :: [Item] -> ([Item],Items)
createItems items = (items2, Items $ listArray (0,length res - 1) (map Defer res))
    where
        (items2,res) = f 0 items
    
        -- number to use next, list of items = (ret,use)
        f :: Int -> [Item] -> ([Item],[Item])
        f i [] = ([],[])
        f i (x:xs) = (x2:a, w [itemTreeStr x2] [] ++ b)
            where
                x2 = x{itemId = w i 0}
                (a,b) = f (w (i+1) i) xs
                w yes no = if wanted x then yes else no

        wanted = not . null . itemName


getItemFromId :: Items -> ItemId -> Item
getItemFromId (Items x) i = fromDefer (x ! i)


