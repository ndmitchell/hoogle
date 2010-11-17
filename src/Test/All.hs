
module Test.All(test) where

import Test.Parse_TypeSig
import Test.Parse_Query


test :: IO ()
test = print $ do
    parse_TypeSig
    parse_Query
