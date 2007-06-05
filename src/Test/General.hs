
module Test.General where


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
