
module Data.Binary.Defer(
    BinaryDefer(..), put,
    BinaryDeferStatic(..),
    defer, defers,
    unit, (<<), (<<~))
    where

import Prelude hiding (catch)
import Control.Exception (catch)

import System.IO
import Foreign(unsafePerformIO)
import Control.Monad
import Data.Bits
import Data.Char


class BinaryDefer a where
    -- take a data value
    -- return a function to save that data value
    -- and one to load it
    bothDefer :: (Handle -> a -> IO [(Int, IO ())], Handle -> IO a)
    bothDefer = (putDefer, get)
    
    putDefer :: Handle -> a -> IO [(Int, IO ())]
    putDefer = fst bothDefer
    
    get :: Handle -> IO a
    get = snd bothDefer

put :: BinaryDefer a => Handle -> a -> IO ()
put hndl x = putDefer hndl x >>= mapM_ f
    where
        f (pos,action) = do
            i <- hGetPos hndl
            hSetPos hndl pos
            hPutInt hndl i
            hSetPos hndl i
            action


class BinaryDefer a => BinaryDeferStatic a where
    -- | Must be a constant, must not examine first argument
    getSize :: a -> Int


type Pending a = (Handle -> Int -> IO [(Int, IO ())], Handle -> IO a)
type Both a = (Handle -> a -> IO [(Int, IO ())], Handle -> IO a)



defer :: (a -> Pending a) -> Both a
defer x = (save, load)
    where
        save hndl value = fst (x value) hndl (-1)

        load hndl = snd (x undefined)  hndl
            

defers :: [a -> Pending a] -> Both a
defers xs = (save, load)
    where
        save hndl value = f $ zip [0::Int ..] xs
            where
                f [] = error "unmatched item to save, or trying to save _|_"
                f ((i,x):xs) = catch (fst (x value) hndl i) (const $ f xs)

        load hndl = do
            i <- hGetInt hndl
            snd ((xs !! i) undefined) hndl



instance BinaryDefer Int where
    putDefer hndl x = hPutInt hndl x >> return []
    get hndl = hGetInt hndl


instance BinaryDefer a => BinaryDefer [a] where
    putDefer hndl xs = hPutInt hndl (length xs) >> concatMapM (putDefer hndl) xs
        where concatMapM f = liftM concat . mapM f
    
    get hndl = do i <- hGetInt hndl; replicateM i (get hndl)

instance BinaryDefer Char where
    putDefer hndl x = hPutChar hndl x >> return []
    get hndl = hGetChar hndl

instance BinaryDefer Bool where
    putDefer hndl x = putDefer hndl (if x then '1' else '0')
    get hndl = get hndl >>= return . (== '1')


instance BinaryDeferStatic Int where getSize _ = 4
instance BinaryDeferStatic Char where getSize _ = 1
instance BinaryDeferStatic Bool where getSize _ = 1


unit :: a -> Pending a
unit f = (\hndl i -> when (i /= -1) (hPutInt hndl i) >> return [], const $ return f)


(<<) :: BinaryDefer a => Pending (a -> b) -> a -> Pending b
(save,load) << x = (\hndl i -> x `seq` do lazy <- save hndl i; s hndl x; return lazy
                   ,\hndl -> do f <- load hndl; x2 <- l hndl; return (f x2))
    where (s,l) = bothDefer

(<<~) :: BinaryDefer a => Pending (a -> b) -> a -> Pending b
(save,load) <<~ x = (\hndl i -> x `seq` do
                          lazy <- save hndl i
                          pos <- hGetPos hndl
                          hPutInt hndl 0
                          return ((pos,put hndl x):lazy)
                    ,\hndl -> do
                          f <- load hndl
                          pos <- hGetInt hndl
                          return $ f $ lazyRead hndl pos
                    )
    where
        lazyRead hndl pos = unsafePerformIO $ do
            hSetPos hndl pos
            get hndl

hGetPos :: Handle -> IO Int
hGetPos = liftM fromInteger . hTell

hSetPos :: Handle -> Int -> IO ()
hSetPos hndl = hSeek hndl AbsoluteSeek . toInteger


-- FROM the Binary module, thanks to the Hac 07 people!

hPutInt :: Handle -> Int -> IO ()
hPutInt hndl w32 = do
    let w4 = (w32 `shiftR` 24)
        w3 = (w32 `shiftR` 16) .&. 0xff
        w2 = (w32 `shiftR`  8) .&. 0xff
        w1 =  w32              .&. 0xff
    putWord8 hndl w1
    putWord8 hndl w2
    putWord8 hndl w3
    putWord8 hndl w4

putWord8 :: Handle -> Int -> IO ()
putWord8 hndl = hPutChar hndl . chr


hGetInt :: Handle -> IO Int
hGetInt hndl = do
    w1 <- getWord8 hndl
    w2 <- getWord8 hndl
    w3 <- getWord8 hndl
    w4 <- getWord8 hndl
    return $! (w4 `shiftL` 24) .|.
              (w3 `shiftL` 16) .|.
              (w2 `shiftL`  8) .|.
              (w1)


getWord8 :: Handle -> IO Int
getWord8 hndl = hGetChar hndl >>= return . ord

