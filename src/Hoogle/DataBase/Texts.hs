
module Hoogle.DataBase.Texts(saveTexts, searchTexts) where

import Hoogle.DataBase.Items
import Hoogle.TextBase.All

import General.All
import System.IO
import Data.List
import Data.Char
import Control.Monad
import Control.Exception

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
                deriving Show

-- the Tree that is created during saveTexts
-- a is the result thingy
data Tree = Tree Int TreeMode [(Char,Tree)] (Int,Int,Int)
            deriving Show

treePosn (Tree a _ _ _) = a

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
        i <- hTell hndl
        () <- assert (fromInteger i == n) $ return ()
        f mode
        mapM_ (hPutInt hndl) [r1,r2,r3]
        mapM_ (writeTree hndl . snd) xs
    where
        f TreeList = hPutInt hndl (length xs) >> mapM_ g xs
            where g (c,t) = hPutByte hndl (ord c) >> hPutInt hndl (treePosn t)
        
        f TreeTrie = hPutInt hndl (-1) >> mapM_ g ['a'..'z']
            where g c = case lookup c xs of
                            Nothing -> hPutInt hndl 0
                            Just t -> hPutInt hndl (treePosn t)


-- takes a tree, and the size of each individual element
-- and computes the size of this data structure
sizeTree :: Tree -> Int
sizeTree (Tree _ mode xs res) = size
    where
        size = sizeInt + f mode + (3 * sizeInt)

        f TreeTrie = 26 * sizeInt
        f TreeList = (sizeByte + sizeInt) * length xs


-- figure out where in a file a tree should go
layoutTree :: Tree -> Int -> (Int, Tree)
layoutTree t@(Tree _ a b c) n = (n2, Tree n a (zip keys vals2) c)
    where
        (n2,vals2) = layoutTrees vals (n + sizeTree t)
        (keys,vals) = unzip b


layoutTrees :: [Tree] -> Int -> (Int, [Tree])
layoutTrees [] n = (n, [])
layoutTrees (x:xs) n = (n3, x2:xs2)
    where
        (n2,x2) = layoutTree x n
        (n3,xs2) = layoutTrees xs n2



saveTexts :: Handle -> [Item] -> IO [Response]
saveTexts hndl xs = do
        i <- hTellInt hndl

        let tree = buildTree 0 strs
            (end, tree2) = layoutTree tree (i + sizeInt)
        
        hPutInt hndl end
        writeTree hndl tree2
        mapM_ outItem ids
        return []
    where
        outItem (a,b) = hPutInt hndl a >> hPutInt hndl b

        tree = buildTree 0 strs
        
        (strs,ids)= unzip $ sortBy cmp items
            where cmp (a,_) (b,_) = compare a b

        items = [(map toLower (drop i s), (idn,i)) | Item{itemId=Just idn, itemName=Just s} <- xs, i <- [0..length s-1]]



searchTexts :: Handle -> String -> IO [Result]
searchTexts hndl search = do
        items <- hGetInt hndl
        res <- f search
        case res of
            Nothing -> return []
            Just (a,b,c) -> do
                hSetPos hndl (items + (sizeInt * 2 * a))
                as <- getResults True  b
                bs <- getResults False (c-b)
                return (as ++ bs)
    where
        getResults b n = replicateM n (do {a <- hGetInt hndl; c <- hGetInt hndl; return $ asResult a b c})
        
        -- item id, match to end, start position
        asResult :: Int -> Bool -> Int -> Result
        asResult idn end pos = Result (TextMatch txt (-1) (-1)) blankItem{itemId=Just idn}
            where txt = if pos == 0 then Prefix else (if end then Suffix else Infix pos)
    
        f xs = do (table,follow) <- readTree hndl
                  case xs of
                      [] -> return $ Just follow
                      (y:ys) -> do
                          case lookup y table of
                              Nothing -> return Nothing
                              Just x -> hSetPos hndl x >> f ys



readTree :: Handle -> IO ( [(Char,Int)], (Int,Int,Int) )
readTree hndl = do
    mode <- hGetInt hndl
    
    res <- if mode == -1
        then do
            res <- replicateM 26 $ hGetInt hndl
            return [(a,b) | (a,b) <- zip ['a'..'z'] res, b /= 0]
        else
            replicateM mode (do {c <- hGetByte hndl; i <- hGetInt hndl; return (chr c,i)})
            
    [a,b,c] <- replicateM 3 $ hGetInt hndl
    return (res, (a,b,c))
