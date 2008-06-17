
module Data.Binary.Defer.Chunk(
    Chunk, newChunk,
    lookupChunk
    ) where

import Control.Monad
import Data.Binary.Defer
import Data.Binary.Defer.Array


chunkSize = 100 :: Int

newtype Chunk a = Chunk (Array (Vector a))
newtype Vector a = Vector {unVector :: [a]}


newChunk :: [a] -> Chunk a
newChunk = Chunk . array . f
    where
        f [] = []
        f xs = Vector a : f b
            where (a,b) = splitAt chunkSize xs


lookupChunk :: (Int,Int) -> Chunk a -> [a]
lookupChunk (from,to) (Chunk xs)
    | m == 0 = f from
    | otherwise = drop (chunkSize - m) (unVector $ xs ! d) ++ f (d+1)
    where
        (d,m) = from `divMod` chunkSize
        (d2,m2) = to `divMod` chunkSize

        f i | i == d2 = if m2 == 0 then [] else take m2 (unVector $ xs ! d2)
            | otherwise = unVector (xs ! i) ++ f i


instance BinaryDefer a => BinaryDefer (Chunk a) where
    put (Chunk a) = put a
    get = get1 Chunk

instance BinaryDefer a => BinaryDefer (Vector a) where
    put (Vector xs) = putInt (length xs) >> mapM_ put xs
    get = do
        i <- getInt
        liftM Vector $ replicateM i get

instance Show a => Show (Chunk a) where
    show (Chunk xs) = unlines $ zipWith f [0..] $ concatMap unVector $ elems xs
        where f i x = show i ++ " = " ++ show x
