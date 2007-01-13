
module Data.Binary.Defer.List(ListDefer) where

import System.IO
import Control.Monad
import Foreign (unsafePerformIO)

import Data.Binary.Defer
import Data.Binary.Defer.Internal


data ListDefer a = ListWrite [a]
                 | ListRead {hndl :: Handle, count :: Int, size :: Int, undef :: a}



instance BinaryDeferStatic a => BinaryDefer (ListDefer a) where
    putDefer hndl (ListWrite xs) = hPutInt hndl (length xs) >> concatMapM (putDefer hndl) xs
        where concatMapM f = liftM concat . mapM f


    get hndl = do
        len <- hGetInt hndl
        let res@(ListRead _ _ size a) = ListRead hndl len (getSize a) undefined
        pos <- hGetPos hndl
        hSetPos hndl (pos + (size * len))
        return res


newListDefer :: BinaryDeferStatic a => [a] -> ListDefer a
newListDefer = ListWrite


-- | Start, Length
readListDefer :: BinaryDeferStatic a => ListDefer a -> Int -> Int -> [a]
readListDefer (ListRead hndl count size _) start len
    | start + len >= count = error "readListDefer, ran off the end"
    | otherwise = unsafePerformIO $ do
        hSetPos hndl (size * start)
        replicateM len (get hndl)
