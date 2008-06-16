-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Defer
-- Copyright   : Neil Mitchell
-- License     : BSD3
-- 
-- Maintainer  : Neil Mitchell <http://www.cs.york.ac.uk/~ndm/>
-- Stability   : unstable
-- Portability : portable to Hugs and GHC. Requires Control.Exception.catch
--
-- Binary serialisation of deferred values
--
-----------------------------------------------------------------------------

module Data.Binary.Defer(
    BinaryDefer(..), put,
    BinaryDeferStatic(..),
    defer,
    Defer(..),
    unit, (<<), (<<~), (<<!)
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
put hndl x = putDefer hndl x >>= mapM_ f . reverse
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


-- have you used any combinators of <<, <<~ or <<!
data PendingNone = PendingNone
data PendingSome = PendingSome

data Pending a b = Pending {psave :: Handle -> Int -> IO [(Int, IO ())], pload :: Handle -> IO a}
type Both a = (Handle -> a -> IO [(Int, IO ())], Handle -> IO a)



deferOne :: (a -> Pending a PendingSome) -> Both a
deferOne x = (save, load)
    where
        save hndl value = psave (x value) hndl (-1)

        load hndl = pload (x undefined)  hndl
            

defer :: [a -> Pending a PendingSome] -> Both a
defer [x] = deferOne x
defer xs = (save, load)
    where
        -- value `seq` is important to that a _|_ in value is
        -- thrown before entering a Catch statement
        -- may still not be safe if multiple levels of combinators
        save hndl value = value `seq` f (zip [0::Int ..] xs)
            where
                f [] = error "unmatched item to save, or trying to save _|_"
                f ((i,x):xs) = catch (psave (x value) hndl i) (const $ f xs)

        load hndl = do
            i <- hGetInt hndl
            pload ((xs !! i) undefined) hndl



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


unit :: a -> Pending a PendingNone
unit f = Pending (\hndl i -> when (i /= -1) (hPutInt hndl i) >> return [])
                 (const $ return f)


(<<!) :: Pending a p -> a -> Pending a PendingSome
(<<!) (Pending save load) val = Pending (val `seq` save) load


(<<) :: BinaryDefer a => Pending (a -> b) p -> a -> Pending b PendingSome
(Pending save load) << x = Pending
        (\hndl i -> x `seq` do lazy <- save hndl i; l <- s hndl x; return (l ++ lazy))
        (\hndl -> do f <- load hndl; x2 <- l hndl; return (f x2))
    where (s,l) = bothDefer

(<<~) :: BinaryDefer a => Pending (a -> b) p -> a -> Pending b PendingSome
(Pending save load) <<~ x = Pending
    (\hndl i -> x `seq` do
        lazy <- save hndl i
        pos <- hGetPos hndl
        hPutInt hndl 0
        return ((pos,put hndl x):lazy))
    (\hndl -> do
        f <- load hndl
        pos <- hGetInt hndl
        return $ f $ lazyRead hndl pos)
    where
        lazyRead hndl pos = unsafePerformIO $ do
            p <- hGetPos hndl
            hSetPos hndl pos
            res <- get hndl
            hSetPos hndl p
            return res


newtype Defer x = Defer {fromDefer :: x}

instance BinaryDefer a => BinaryDefer (Defer a) where
    bothDefer = defer [\ ~(Defer a) -> unit Defer <<~ a]

instance Show a => Show (Defer a) where
    show (Defer a) = "(Defer " ++ show a ++ ")"

instance BinaryDefer a => BinaryDeferStatic (Defer a) where
    getSize _ = 4
