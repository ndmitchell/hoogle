
module Hoogle.Store.Array(
    (!), Array, array, elems, arraySize
    ) where

import Data.Array hiding ((!), Array, array, elems)
import qualified Data.Array as A
import Hoogle.Store.All
import Data.BinaryRaw
import System.IO.Unsafe
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader


newtype Array a = Array (A.Array Int a)

array :: [a] -> Array a
array xs = Array $ listArray (0, length xs - 1) xs

(!) :: Array a -> Int -> a
(!) (Array a) i = a A.! i

elems :: Array a -> [a]
elems (Array a) = A.elems a


arraySize (Array a) = 1 + y - x
    where (x,y) = bounds a

instance Functor Array where
    fmap f (Array x) = Array (fmap f x)

instance Eq a => Eq (Array a) where
    Array x == Array y = x == y

instance Show a => Show (Array a) where
    show (Array x) = show x

instance BinaryDefer a => BinaryDefer (Array a) where
    put (Array xs) = putDefer $ do
        putInt $ snd $ bounds xs
        mapM_ putFixed (A.elems xs)

    get = getDefer ans
        where
            ans = do
                n <- getInt
                h <- asks fst
                s <- ask
                i <- liftIO $ hGetPos h
                let f j = unsafePerformIO $ do
                             hSetPos h (i + toInteger (sz*j))
                             runReaderT getFixed s
                return $ Array $ listArray (0,n) $ map f [0..n]

            unwrap = undefined :: DeferGet (Array a) -> a
            sz = size $ unwrap ans
