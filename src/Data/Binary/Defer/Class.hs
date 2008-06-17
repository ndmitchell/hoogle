
module Data.Binary.Defer.Class(BinaryDefer(..)) where

import Control.Monad
import Data.Binary.Defer.Monad


---------------------------------------------------------------------
-- BinaryDefer

class BinaryDefer a where
    put :: a -> DeferPut ()
    get :: DeferGet a


instance BinaryDefer Int where
    put = putInt
    get = getInt

instance BinaryDefer Char where
    put = putChr
    get = getChr

instance BinaryDefer Bool where
    put x = putChr (if x then '1' else '0')
    get = liftM (== '1') getChr


-- strategy: write out in 100 byte chunks, where each successive
-- chunk is lazy, but the first is not
instance BinaryDefer a => BinaryDefer [a] where
    put xs | null b = putByte (length a) >> mapM_ put a
           | otherwise = putByte (-1) >> mapM_ put a >> putDefer (put b)
        where (a,b) = splitAt 100 xs

    get = do
        i <- getByte
        if i /= (-1) then do
            replicateM i get
         else do
            xs <- replicateM 100 get
            ys <- getDefer get
            return (xs++ys)


---------------------------------------------------------------------
-- BinaryDeferStatic

class BinaryDeferStatic a where
    size :: a -> Int

instance BinaryDeferStatic Int  where size _ = 4
instance BinaryDeferStatic Char where size _ = 1
instance BinaryDeferStatic Bool where size _ = 1
