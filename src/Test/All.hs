
module Test.All(test) where

import Test.Parse_TypeSig
import Test.Parse_Query
import Test.Docs
import Test.BWT_FM


test :: IO ()
test = do
    parse_TypeSig
    parse_Query
    docs
    bwt_fm
