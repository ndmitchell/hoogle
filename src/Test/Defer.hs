
-- Note: not included in the automated testing as it creates
--       a temporary file

module Test.Defer where

import Control.Monad
import Data.Binary.Raw
import Data.Binary.Defer
import Data.Binary.Defer.Array
import System.IO


test :: IO ()
test = do
    h <- openBinaryFile "temp.bin" ReadWriteMode
    runDeferPut h (put value)
    hSetPos h 0
    v <- runDeferGet h get
    when (v /= value) $
        error $ "wrong, got:\n" ++ show v
    putStrLn "Success"


value = (
            -- test the atoms
            ((),12::Int,True,'a'),
            -- test compound types
            ("hello",nothing,Just 'a',right True,left "test"),
            -- test long strings
            take 2000 $ cycle ['a'..'z'],
            -- test the custom types
            array [(i::Int,True,"neil") | i <- [1..40]],
            -- check things are not messed up previously
            "final check"
        )


nothing = Nothing :: Maybe ()
left = Left :: a -> Either a ()
right = Right :: a -> Either () a
