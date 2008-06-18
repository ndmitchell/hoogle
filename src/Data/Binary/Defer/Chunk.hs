
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
lookupChunk (from,to) (Chunk xs) =
        drop m $ f d (1 + m + to-from)
    where
        (d,m) = from `divMod` chunkSize

        f i n | n == 0 = []
              | n < chunkSize = take n $ unVector $ xs ! i
              | otherwise = unVector (xs ! i) ++ f (i+1) (n-chunkSize)


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
