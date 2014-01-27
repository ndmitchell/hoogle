
module Test.General where

import Control.Monad

parseTest f input output =
    case f input of
        Left x -> err "Parse failed" (show x)
        Right x -> when (x /= output) $ err "Parse not equal" (show x)
    where
        err pre post = error $ pre ++ ":\n  " ++ input ++ "\n  " ++ show output ++ "\n  " ++ post

parseTest2 f input output =
    case f input of
        (x:xs,_) -> err "Parse failed" (show x)
        ([],  x) -> when (x /= output) $ err "Parse not equal" (show x)
    where
        err pre post = error $ pre ++ ":\n  " ++ input ++ "\n  " ++ show output ++ "\n  " ++ post
