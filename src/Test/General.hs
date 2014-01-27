
module Test.General(parseTest) where

import Control.Monad

parseTest :: (Show a, Show e, Eq a) => (String -> Either e a) -> String -> a -> IO ()
parseTest f input output =
    case f input of
        Left x -> err "Parse failed" (show x)
        Right x -> when (x /= output) $ err "Parse not equal" (show x)
    where
        err pre post = error $ pre ++ ":\n  " ++ input ++ "\n  " ++ show output ++ "\n  " ++ post
