{-# LANGUAGE OverloadedStrings #-}

module Test.BWT_FM(bwt_fm) where

import Test.General
import Test.QuickCheck
import General.BurrowsWheeler


bwt_fm = do
    compress "tomorrow and tomorrow and tomorrow" === (31,"wwwdd  nnoooaatttmmmrrrrrrooo  ooo")
    decompress (31,"wwwdd  nnoooaatttmmmrrrrrrooo  ooo") === "tomorrow and tomorrow and tomorrow"
    quickCheck $ \x -> decompress (compress x) == x
