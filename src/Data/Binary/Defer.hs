
module Data.Binary.Defer(
    BinaryDefer(..), put,
    BinaryDeferStatic(..),
    defer, defers,
    Lazy(..),
    unit, (<<), (<<~)
    ) where

import Prelude hiding (catch)
import Control.Exception (catch)

import System.IO
import Foreign(unsafePerformIO)
import Control.Monad
import Data.Binary.Defer.Internal


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


newtype Lazy x = Lazy {fromLazy :: x}

instance BinaryDefer a => BinaryDefer (Lazy a) where
    putDefer hndl (Lazy x) = do
        i <- hGetPos hndl
        hPutInt hndl 0
        return [(i, put hndl x)]

    get hndl = do
            i <- hGetInt hndl
            return $ Lazy $ unsafePerformIO $ f i
        where
            f i = hSetPos hndl i >> get hndl

instance BinaryDefer a => BinaryDeferStatic (Lazy a) where
    getSize _ = 4
