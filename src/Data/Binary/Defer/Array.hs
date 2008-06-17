
module Data.Binary.Defer.Array(
    (!), Array, array, elems
    ) where

import Data.Array hiding ((!), Array, array, elems)
import qualified Data.Array as A
import Data.Binary.Defer
import Data.Binary.Raw
import System.IO.Unsafe
import Control.Monad.Reader


newtype Array a = Array (A.Array Int a)

array :: [a] -> Array a
array xs = Array $ listArray (0, length xs - 1) xs

(!) :: Array a -> Int -> a
(!) (Array a) i = a A.! i

elems :: Array a -> [a]
elems (Array a) = A.elems a


instance Functor Array where
    fmap f (Array x) = Array (fmap f x)


instance BinaryDefer a => BinaryDefer (Array a) where
    put (Array xs) = do
        putInt $ snd $ bounds xs
        mapM_ (putDefer . put) (A.elems xs)

    get = do
        n <- getInt
        h <- ask
        i <- lift $ hGetPos h
        return $ Array $ listArray (0,n) $ map (f h i) [0..n]
        where
            f h i j = unsafePerformIO $ do
                hSetPos h (i + 4*j)
                runDeferGet h (getDefer get)
