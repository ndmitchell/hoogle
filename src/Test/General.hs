{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Test.General where


---------------------------------------------------------------------
-- The Test Monad

type Test a = IO a

pass :: Test ()
pass = return ()


parseTest f input output =
    case f input of
        Left x -> err "Parse failed" (show x)
        Right x -> if x == output then pass else
                   err "Parse not equal" (show x)
    where
        err pre post = error $ pre ++ ":\n  " ++ input ++ "\n  " ++ show output ++ "\n  " ++ post

parseTest2 f input output =
    case f input of
        (x:xs,_) -> err "Parse failed" (show x)
        ([],  x) -> if x == output then pass else
                    err "Parse not equal" (show x)
    where
        err pre post = error $ pre ++ ":\n  " ++ input ++ "\n  " ++ show output ++ "\n  " ++ post
