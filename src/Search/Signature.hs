{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Search.Signature(
    signatureCreate, signatureMerge, signatureSearch
    ) where

import Search.Type
import General.Base
import System.FilePath
import Hoogle.Type.TypeSig
import Hoogle.Type.Item


signatureCreate :: FilePath -> Package -> [Package] -> [Fact] -> [(Key, TypeSig)] -> IO ()
signatureCreate dir p pDep sDep xs = do
    deps <- fmap (concatMap read) $ forM pDep $ \d -> readFile (dir </> show d <.> "dep")
    writeFile (dir </> show p <.> "fact") $ show (deps ++ sDep :: [Fact])
    writeFile (dir </> show p <.> "sig") $ show xs


signatureMerge :: FilePath -> [Package] -> IO ()
signatureMerge = error "signatureMerge"


signatureSearch :: FilePath -> Package -> TypeSig -> IO [(Double, (Key, [Int]))]
signatureSearch dir p t = do
    facts <- fmap read $ readFile (dir </> show p <.> "fact") :: IO [Fact]
    sigs <- fmap read $ readFile (dir </> show p <.> "sig")
    let f (k :: Key, tt :: TypeSig) = fmap (, (k,[])) $ quality t facts tt
    return $ reverse $ sortBy (compare `on` fst) $ mapMaybe f sigs

quality :: TypeSig -> [Fact] -> TypeSig -> Maybe Double
quality query facts db
    | query == db = Just 1
    | otherwise = Just 0


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
