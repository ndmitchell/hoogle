{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Test.General where


---------------------------------------------------------------------
-- The Test Monad

data Test a = Test

instance Monad Test where
    a >> b = a `seq` b

instance Show (Test a) where
    show x = x `seq` "All tests passed"

pass :: Test ()
pass = Test


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


---------------------------------------------------------------------
-- The List Monad

data List a b = List {fromList :: [a]}

instance Monad (List a) where
    List a >> List b = List (a++b)

pair :: a -> b -> List (a,b) c
pair a b = List [(a,b)]

