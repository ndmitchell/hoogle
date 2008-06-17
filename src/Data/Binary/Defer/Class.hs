
module Data.Binary.Defer.Class where

import Control.Monad
import Data.Binary.Defer.Monad
import Data.Binary.Raw


---------------------------------------------------------------------
-- BinaryDefer

class BinaryDefer a where
    put :: a -> DeferPut ()
    get :: DeferGet a


get0 f = return f
get1 f = do x1 <- get; return (f x1)
get2 f = do x1 <- get; x2 <- get; return (f x1 x2)
get3 f = do x1 <- get; x2 <- get; x3 <- get; return (f x1 x2 x3)
get4 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; return (f x1 x2 x3 x4)
get5 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; return (f x1 x2 x3 x4 x5)
get6 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; return (f x1 x2 x3 x4 x5 x6)


instance BinaryDefer Int where
    put = putInt
    get = getInt

instance BinaryDefer Char where
    put = putChr
    get = getChr

instance BinaryDefer Bool where
    put x = putChr (if x then '1' else '0')
    get = liftM (== '1') getChr


instance BinaryDefer () where
    put () = return ()
    get = return ()

instance (BinaryDefer a, BinaryDefer b) => BinaryDefer (a,b) where
    put (a,b) = put a >> put b
    get = get2 (,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c) =>
    BinaryDefer (a,b,c) where
    put (a,b,c) = put a >> put b >> put c
    get = get3 (,,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c, BinaryDefer d) =>
    BinaryDefer (a,b,c,d) where
    put (a,b,c,d) = put a >> put b >> put c >> put d
    get = get4 (,,,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c, BinaryDefer d,
    BinaryDefer e) => BinaryDefer (a,b,c,d,e) where
    put (a,b,c,d,e) = put a >> put b >> put c >> put d >> put e
    get = get5 (,,,,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c, BinaryDefer d,
    BinaryDefer e, BinaryDefer f) => BinaryDefer (a,b,c,d,e,f) where
    put (a,b,c,d,e,f) = put a >> put b >> put c >> put d >> put e >> put f
    get = get6 (,,,,,)


instance BinaryDefer a => BinaryDefer (Maybe a) where
    put Nothing = putByte 0
    put (Just a) = putByte 1 >> put a

    get = do i <- getByte
             case i of
                0 -> get0 Nothing
                1 -> get1 Just

instance (BinaryDefer a, BinaryDefer b) => BinaryDefer (Either a b) where
    put (Left a) = putByte 0 >> put a
    put (Right a) = putByte 1 >> put a
    
    get = do i <- getByte
             case i of
                0 -> get1 Left
                1 -> get1 Right


-- strategy: write out in 100 byte chunks, where each successive
-- chunk is lazy, but the first is not
instance BinaryDefer a => BinaryDefer [a] where
    put xs | null b = putByte (length a) >> mapM_ put a
           | otherwise = putByte maxByte >> mapM_ put a >> putDefer (put b)
        where (a,b) = splitAt 100 xs

    get = do
        i <- getByte
        if i /= maxByte then do
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
