
module Test.All(test) where

import Test.Parse_TypeSig
import Test.Parse_Query
import Test.Docs


test :: IO ()
test = print $ do
    parse_TypeSig
    parse_Query
    docs
