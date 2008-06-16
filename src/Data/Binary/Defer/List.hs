
module Data.Binary.Defer.List(ListDefer, newListDefer, readListDefer) where

import System.IO
import Control.Monad
import Foreign (unsafePerformIO)

import Data.Binary.Defer
import Data.Binary.Defer.Internal


data ListDefer a = ListWrite [a]
                 | ListRead {hndl :: Handle, pos :: Int, count :: Int, size :: Int, undef :: a}


instance (BinaryDeferStatic  a, Show a) => Show (ListDefer a) where
    show (ListWrite a) = "(ListDefer " ++ show a ++ ")"
    show x = "(ListDefer " ++ show (readListDefer x 0 (count x)) ++ ")"


instance BinaryDeferStatic a => BinaryDefer (ListDefer a) where
    putDefer hndl (ListWrite xs) = hPutInt hndl (length xs) >> concatMapM (putDefer hndl) xs
        where concatMapM f = liftM concat . mapM f


    get hndl = do
        len <- hGetInt hndl
        pos <- hGetPos hndl
        let res = ListRead hndl pos len (getSize (undef res)) undefined
        hSetPos hndl (pos + (size res * len))
        return res


newListDefer :: BinaryDeferStatic a => [a] -> ListDefer a
newListDefer = ListWrite


-- | Start, Length
readListDefer :: BinaryDeferStatic a => ListDefer a -> Int -> Int -> [a]
readListDefer (ListRead hndl pos count size _) start len
    | start + len > count = error "readListDefer, ran off the end"
    | otherwise = unsafePerformIO $ do
        p <- hGetPos hndl
        hSetPos hndl (pos + (size * start))
        res <- replicateM len (get hndl)
        hSetPos hndl p
        return res
