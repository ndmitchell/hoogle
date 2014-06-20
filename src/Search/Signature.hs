
module Search.Signature(
    signatureCreate, signatureMerge, signatureSearch
    ) where

import Search.Type

signatureCreate :: FilePath -> Package -> [(Key, Stm)] -> IO ()
signatureCreate = undefined


signatureMerge :: FilePath -> [Package] -> IO ()
signatureMerge = undefined


signatureSearch :: FilePath -> [Package] -> Typ -> [(Key, [Int])]
signatureSearch = undefined


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
