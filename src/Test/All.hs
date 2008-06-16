
module Test.All(test) where

import Test.Parse_TypeSig
import Test.Parse_Query
import Test.Parse_TextBase


test :: IO ()
test = print $ do
    parse_TypeSig
    parse_Query
    parse_TextBase
