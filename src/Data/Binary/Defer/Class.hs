
module Data.Binary.Defer.Class where

import Control.Monad
import Data.Binary.Defer.Monad


---------------------------------------------------------------------
-- BinaryDefer

class BinaryDefer a where
    put :: a -> DeferPut ()
    get :: DeferGet a

get0 = return
get1 f = do x1 <- get; return (f x1)
get2 f = do x1 <- get; x2 <- get; return (f x1 x2)
get3 f = do x1 <- get; x2 <- get; x3 <- get; return (f x1 x2 x3)
get4 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; return (f x1 x2 x3 x4)
get5 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; return (f x1 x2 x3 x4 x5)

instance BinaryDefer Int where
    put = putInt
    get = getInt

instance BinaryDefer Char where
    put = putChr
    get = getChr

instance BinaryDefer Bool where
    put x = putChr (if x then '1' else '0')
    get = liftM (== '1') getChr

instance BinaryDefer a => BinaryDefer (Maybe a) where
    put Nothing = putByte 0
    put (Just a) = putByte 1 >> put a

    get = do i <- getByte
             if i == 1 then get1 Just else get0 Nothing
   

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
