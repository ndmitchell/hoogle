
module Hoogle.DataBase.Texts(saveTexts) where

import Hoogle.DataBase.Items
import Hoogle.TextBase.All

import General.Binary
import System.IO
import Data.List
import Data.Char

{-
DESIGN 2

Have a list of the sorted names

[ap],_id_,1
[map],_id_,0
[p],_id_,2

_id_ is the id (int) of each element
the names are sorted

each is 8 bytes long

The tree then is defined as:

    List (count::Int) (items::[(Char,Int)]) (results::(Int,Int,Int))
|
    Trie (items::[Int*26]) (results::(Int,Int,Int))
    
    for Trie 0 = go nowhere, no results
    
    
Results are given as
    start position in the list
    count of exact items
    count of all

    0,0,0 == no results

-}


-- True = Trie, False = List
data TreeMode = TreeTrie | TreeList

-- the Tree that is created during saveTexts
-- a is the result thingy
data Tree = Tree Int TreeMode [(Char,Tree)] (Int,Int,Int)

treeSize (Tree a _ _ _) = a

-- input list must be sorted already!
buildTree :: Int -> [String] -> Tree
buildTree n xs = Tree 0 (makeMode res) res (n,length blank,length xs)
    where
        (blank, non) = span null xs
        
        res = f (n + length blank) ys
        
        ys = groupBy cmp non
            where cmp a b = head a == head b

        f n [] = []
        f n (x:xs) = (head $ head x, buildTree n $ map tail x) : f (n + length x) xs
        
        makeMode xs = if all (isLetter . fst) xs && length xs > 10
                      then TreeTrie else TreeList

        isLetter x = x >= 'a' && x <= 'z'


-- id of the item, position in the item
writeTree :: Handle -> Tree -> IO ()
writeTree hndl (Tree n mode xs (r1,r2,r3)) = do
        f mode
        mapM_ (hPutInt hndl) [r1,r2,r2]
    where
        mysize = n - sum (map (treeSize . snd) xs)
        sxs = getSize n xs
        
        getSize n [] = []
        getSize n (x:xs) = (n,x) : getSize (n + treeSize (snd x)) xs
    
        f TreeList = hPutInt hndl (length xs) >> mapM_ g sxs
            where g (i,(c,_)) = hPutByte hndl (ord c) >> hPutInt hndl i
        
        f TreeTrie = hPutInt hndl (-1) >> mapM_ g ['a'..'z']
            where g c = case [i | (i,(c2,_)) <- sxs] of
                            [] -> hPutInt hndl 0
                            [i] -> hPutInt hndl i


-- takes a tree, and the size of each individual element
-- and computes the size of that subtree
sizeTree :: Tree -> Tree
sizeTree (Tree _ mode xs res) = Tree size mode xs2 res
    where
        size = sizeInt + sz + f mode
        
        xs2 = [(a,sizeTree b) | (a,b) <- xs]
        sz = sum $ map (treeSize . snd) xs2
        
        f TreeTrie = 26 * sizeInt
        f TreeList = sizeInt + ((sizeByte + sizeInt) * length xs)
        



-- take in (moduleId, item) -> [(itemId, item, dbitem)]
saveTexts :: Handle -> [(Int, Item, DBItem)] -> IO [String]
saveTexts hndl xs = do
        i <- hTellInt hndl
        hPutInt hndl (i + ntree)
        writeTree hndl tree
        mapM_ outItem ids
        return []
    where
        outItem (a,b) = hPutInt hndl a >> hPutInt hndl b
    
        ntree = treeSize tree
        tree = sizeTree $ buildTree 0 strs
        
        (strs,ids)= unzip $ sortBy cmp items
            where cmp (a,_) (b,_) = compare a b

        items = [(map toLower (drop i s), (idn,i)) | (idn,_,DBItem s _) <- xs, i <- [0..length s-1]]

