
module Data.Binary.Defer.Class where

import Control.Monad
import Data.Binary.Defer.Monad
import Data.Binary.Raw
import Data.ByteString(ByteString)
import General.Util(splitAtLength)

---------------------------------------------------------------------
-- BinaryDefer

class BinaryDefer a where
    put :: a -> DeferPut ()
    get :: DeferGet a

    size :: a -> Int
    size _ = 4
    
    putFixed :: a -> DeferPut ()
    putFixed = putDefer . put

    getFixed :: DeferGet a
    getFixed = getDefer get


errorDeferGet :: String -> a
errorDeferGet typ = error $ "BinaryDefer.get(" ++ typ ++ "), corrupt database"


get0 f = return f
get1 f = do x1 <- get; return (f x1)
get2 f = do x1 <- get; x2 <- get; return (f x1 x2)
get3 f = do x1 <- get; x2 <- get; x3 <- get; return (f x1 x2 x3)
get4 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; return (f x1 x2 x3 x4)
get5 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; return (f x1 x2 x3 x4 x5)
get6 f = do x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; return (f x1 x2 x3 x4 x5 x6)


getFixed0 f = return f
getFixed1 f = do x1 <- getFixed; return (f x1)
getFixed2 f = do x1 <- getFixed; x2 <- getFixed; return (f x1 x2)
getFixed3 f = do x1 <- getFixed; x2 <- getFixed; x3 <- getFixed; return (f x1 x2 x3)
getFixed4 f = do x1 <- getFixed; x2 <- getFixed; x3 <- getFixed; x4 <- getFixed; return (f x1 x2 x3 x4)
getFixed5 f = do x1 <- getFixed; x2 <- getFixed; x3 <- getFixed; x4 <- getFixed; x5 <- getFixed; return (f x1 x2 x3 x4 x5)
getFixed6 f = do x1 <- getFixed; x2 <- getFixed; x3 <- getFixed; x4 <- getFixed; x5 <- getFixed; x6 <- getFixed; return (f x1 x2 x3 x4 x5 x6)


put0 = return () :: DeferPut ()
put1 x1 = put x1
put2 x1 x2 = put x1 >> put x2
put3 x1 x2 x3 = put x1 >> put x2 >> put x3
put4 x1 x2 x3 x4 = put x1 >> put x2 >> put x3 >> put x4
put5 x1 x2 x3 x4 x5 = put x1 >> put x2 >> put x3 >> put x4 >> put x5
put6 x1 x2 x3 x4 x5 x6 = put x1 >> put x2 >> put x3 >> put x4 >> put x5 >> put x6


putFixed0 = return () :: DeferPut ()
putFixed1 x1 = putFixed x1
putFixed2 x1 x2 = putFixed x1 >> putFixed x2
putFixed3 x1 x2 x3 = putFixed x1 >> putFixed x2 >> putFixed x3
putFixed4 x1 x2 x3 x4 = putFixed x1 >> putFixed x2 >> putFixed x3 >> putFixed x4
putFixed5 x1 x2 x3 x4 x5 = putFixed x1 >> putFixed x2 >> putFixed x3 >> putFixed x4 >> putFixed x5
putFixed6 x1 x2 x3 x4 x5 x6 = putFixed x1 >> putFixed x2 >> putFixed x3 >> putFixed x4 >> putFixed x5 >> putFixed x6


instance BinaryDefer Int where
    put = putInt
    get = getInt
    size _ = 4
    putFixed = put
    getFixed = get

instance BinaryDefer Char where
    put = putChr
    get = getChr
    size _ = 1
    putFixed = put
    getFixed = get

instance BinaryDefer Bool where
    put x = putChr (if x then '1' else '0')
    get = liftM (== '1') getChr
    size _ = 1
    putFixed = put
    getFixed = get


instance BinaryDefer () where
    put () = return ()
    get = return ()
    size _ = 0
    putFixed = put
    getFixed = get

instance (BinaryDefer a, BinaryDefer b) => BinaryDefer (a,b) where
    put (a,b) = put2 a b
    get = get2 (,)
    size x = let ~(a,b) = x in size a + size b
    putFixed (a,b) = putFixed2 a b
    getFixed = getFixed2 (,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c) =>
    BinaryDefer (a,b,c) where
    put (a,b,c) = put3 a b c
    get = get3 (,,)
    size x = let ~(a,b,c) = x in size a + size b + size c
    putFixed (a,b,c) = putFixed3 a b c
    getFixed = getFixed3 (,,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c, BinaryDefer d) =>
    BinaryDefer (a,b,c,d) where
    put (a,b,c,d) = put4 a b c d
    get = get4 (,,,)
    size x = let ~(a,b,c,d) = x in size a + size b + size c + size d
    putFixed (a,b,c,d) = putFixed4 a b c d
    getFixed = getFixed4 (,,,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c, BinaryDefer d,
    BinaryDefer e) => BinaryDefer (a,b,c,d,e) where
    put (a,b,c,d,e) = put5 a b c d e
    get = get5 (,,,,)
    size x = let ~(a,b,c,d,e) = x in size a + size b + size c + size d + size e
    putFixed (a,b,c,d,e) = putFixed5 a b c d e
    getFixed = getFixed5 (,,,,)

instance (BinaryDefer a, BinaryDefer b, BinaryDefer c, BinaryDefer d,
    BinaryDefer e, BinaryDefer f) => BinaryDefer (a,b,c,d,e,f) where
    put (a,b,c,d,e,f) = put6 a b c d e f
    get = get6 (,,,,,)
    size x = let ~(a,b,c,d,e,f) = x in size a + size b + size c + size d + size e + size f
    putFixed (a,b,c,d,e,f) = putFixed6 a b c d e f
    getFixed = getFixed6 (,,,,,)

instance BinaryDefer a => BinaryDefer (Maybe a) where
    put Nothing = putByte 0
    put (Just a) = putByte 1 >> put a

    get = do i <- getByte
             case i of
                0 -> get0 Nothing
                1 -> get1 Just
                _ -> errorDeferGet "Maybe"

instance (BinaryDefer a, BinaryDefer b) => BinaryDefer (Either a b) where
    put (Left a) = putByte 0 >> put a
    put (Right a) = putByte 1 >> put a
    
    get = do i <- getByte
             case i of
                0 -> get1 Left
                1 -> get1 Right
                _ -> errorDeferGet "Either"


-- strategy: write out in 100 byte chunks, where each successive
-- chunk is lazy, but the first is not
instance BinaryDefer a => BinaryDefer [a] where
    put xs = putList xs

    get = do
        i <- getByte
        if i /= maxByte then do
            replicateM i get
         else do
            xs <- replicateM 100 get
            ys <- getDefer get
            return (xs++ys)


-- Extracted to allow putList to appear on the profile
putList :: BinaryDefer a => [a] -> DeferPut ()
putList xs | null b = putByte n >> mapM_ put a
           | otherwise = putByte maxByte >> mapM_ put a >> putDefer (put b)
        where (n,a,b) = splitAtLength 100 xs


instance BinaryDefer ByteString where
    put = putDefer . putByteString
    get = getDefer getByteString
    putFixed = put
    getFixed = get


newtype Defer a = Defer a

fromDefer :: Defer a -> a
fromDefer (Defer x) = x

instance BinaryDefer a => BinaryDefer (Defer a) where
    put (Defer x) = putDefer $ put x
    get = getDefer $ liftM Defer get
    putFixed = put
    getFixed = get


