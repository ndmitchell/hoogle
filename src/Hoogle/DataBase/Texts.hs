
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

[map],_id_,0
[ap],_id_,1
[p],_id_,2

Then the tree points at a start and an end, with a number
saying how many are exact

-}


-- True = Trie, False = List
data TreeMode a = TreeNone
                | TreeTail String a
                | TreeList [(Char, Tree a)]
                | TreeTrie [(Char, Tree a)]


-- the Tree that is created during saveTexts
-- a is the result thingy
data Tree a = Tree Int (TreeMode a) [a]


-- input list must be sorted already!
buildTree :: [(String, a)] -> Tree a
buildTree xs = Tree 0 (makeMode $ map f ys) (map snd blank)
    where
        (blank, non) = span (null . fst) xs
        
        ys = groupBy cmp xs
            where cmp (a:_,_) (b:_,_) = a == b
        
        f zs@((c,_):_) = (head c, buildTree [(tail a,b) | (a,b) <- zs])
        
        makeMode [] = TreeNone
        makeMode x | length xs == 1 = let [(a,b)] = xs in TreeTail a b
                   | all (isLetter . fst) x && length x > 10 = TreeTrie x
                   | otherwise = TreeList x

        isLetter x = x >= 'a' && x <= 'z'


-- id of the item, position in the item
writeTree :: Handle -> Tree (Int, Int) -> IO ()
writeTree hndl (Tree n xs ys) = return ()
    where
        f TreeNone = hPutByte hndl 0
        
        f (TreeTail a b) = do
            hPutByte hndl 1
            hPutStr hndl a
            
        f (TreeList xs) = do
            i <- hTellInt hndl
            let nxs = length xs
            hPutByte hndl 2
            hPutInt hndl nxs
            g (i+sizeByte+sizeInt + ((sizeByte+sizeInt) * nxs)) xs
            mapM_ (writeTree hndl . snd) xs
            where
                g i [] = return ()
                g i ((c,Tree n _ _):xs) = do
                    hPutByte hndl (ord c)
                    hPutInt hndl i
                    g (i + n) xs
        
--        f (TreeTrie 
            
        


-- takes a tree, and the size of each individual element
-- and computes the size of that subtree
sizeTree :: Int -> Tree a -> Tree a
sizeTree n (Tree _ m xs) = Tree (sizeByte + sizeInt + (length xs * n) + f m2) m2 xs
    where
        m2 = case m of
                TreeList x -> TreeList (g x)
                TreeTrie x -> TreeTrie (g x)
                x -> x
    
        g xs = [(a,sizeTree n b) | (a,b) <- xs]
    
        f (TreeNone) = 0
        f (TreeTail s x) = sizeStr s + n
        f (TreeList xs) = sizeInt + ((sizeByte + sizeInt) * length xs) + h xs
        f (TreeTrie xs) = (sizeInt * 26) + h xs
        
        h xs = sum [n | (_,Tree n _ _) <- xs]
        
        



-- take in (moduleId, item) -> [(itemId, item, dbitem)]
saveTexts :: Handle -> [(Int, Item, DBItem)] -> IO [String]
saveTexts hndl xs = writeTree hndl tree >> return []
    where
        tree = sizeTree 8 $ buildTree $ sortBy cmp items
            where cmp (a,_) (b,_) = compare a b

        items = [(map toLower (drop i s), (idn,i)) | (idn,_,DBItem s _) <- xs, i <- [0..length s-1]]

