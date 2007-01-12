
module Data.Binary.Defer(
    BinaryDefer(..), serial,
    unit, (<<), (<<~))
    where

import Prelude hiding (catch)
import Control.Exception (catch)

import System.IO
import Foreign(unsafePerformIO)
import Control.Monad



class BinaryDefer a where
    -- take a data value
    -- return a function to save that data value
    -- and one to load it
    bothDefer :: (Handle -> a -> IO (), Handle -> IO a)
    bothDefer = (putDefer, getDefer)
    
    putDefer :: Handle -> a -> IO ()
    putDefer = fst bothDefer
    
    getDefer :: Handle -> IO a
    getDefer = snd bothDefer


serial :: [a -> (Handle -> Int -> IO (), Handle -> IO a)] -> (Handle -> a -> IO (), Handle -> IO a)
serial xs = (save, load)
    where
        save hndl value = f $ zip [0::Int ..] xs
            where
                f [] = error "unmatched item to save, or trying to save _|_"
                f ((i,x):xs) = catch (fst (x value) hndl i) (const $ f xs)

        load hndl = do
            i <- getDefer hndl
            snd ((xs !! i) undefined) hndl



instance BinaryDefer Int where
    putDefer hndl x = hPutStr hndl (replicate (10 - length s) ' ' ++ s)
        where s = show x

    getDefer hndl = replicateM 10 (hGetChar hndl) >>= return . read


instance BinaryDefer a => BinaryDefer [a] where
    putDefer hndl xs = putDefer hndl (length xs) >> mapM_ (putDefer hndl) xs
    
    getDefer hndl = do i <- getDefer hndl; print i; replicateM i (getDefer hndl)

instance BinaryDefer Char where
    putDefer hndl x = putDefer hndl (fromEnum x)
    getDefer hndl = liftM toEnum $ getDefer hndl

instance BinaryDefer Bool where
    putDefer hndl x = putDefer hndl (fromEnum x)
    getDefer hndl = liftM toEnum $ getDefer hndl


unit :: a -> (Handle -> Int -> IO (), Handle -> IO a)
unit f = (putDefer, const $ return f)


(<<) :: BinaryDefer a => (Handle -> Int -> IO (), Handle -> IO (a -> b)) -> a -> (Handle -> Int -> IO (), Handle -> IO b)
sl << x = combine sl x bothDefer

(<<~) :: BinaryDefer a => (Handle -> Int -> IO (), Handle -> IO (a -> b)) -> a -> (Handle -> Int -> IO (), Handle -> IO b)
sl <<~ x = combine sl x (lazyWrite, lazyRead)
    where
        (save,load) = bothDefer
        
        lazyWrite hndl x = do
            begin <- hGetPos hndl
            putDefer hndl (0 :: Int)
            save hndl x
            end <- hGetPos hndl
            hSetPos hndl begin
            putDefer hndl end
            hSetPos hndl end
            
        lazyRead hndl = do
            end <- getDefer hndl
            begin <- hGetPos hndl
            hSetPos hndl end
            return $ unsafePerformIO (hSetPos hndl begin >> load hndl)

        hGetPos :: Handle -> IO Int
        hGetPos = liftM fromInteger . hTell
        
        hSetPos :: Handle -> Int -> IO ()
        hSetPos hndl = hSeek hndl AbsoluteSeek . toInteger

combine :: (Handle -> Int -> IO (), Handle -> IO (a -> b)) -> a -> (Handle -> a -> IO (), Handle -> IO a) -> (Handle -> Int -> IO (), Handle -> IO b)
combine (save,load) x (s,l) = (\hndl i -> x `seq` save hndl i >> s hndl x, \hndl -> do f <- load hndl; x2 <- l hndl; return (f x2))
