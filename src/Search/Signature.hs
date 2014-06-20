
module Search.Signature(
    signatureCreate, signatureMerge, signatureSearch
    ) where

import Search.Type

signatureCreate :: FilePath -> Package -> [Package] -> [(Key, Stm)] -> IO ()
signatureCreate dir out deps xs = return ()


signatureMerge :: FilePath -> [Package] -> IO ()
signatureMerge = error "signatureMerge"


signatureSearch :: FilePath -> Package -> Typ -> IO [(Double, (Key, [Int]))]
signatureSearch = error "signatureSearch"


{-
Type search

Problem is that it doesn't decompose nicely...

type aliases - can expand through all of those, but ugly

Given a b c, what do you want 

type aliases
instances


Suggestion is a search database


data Type = Alias String Type
          | 
-}
