{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable #-}

module Output.Names(writeNames, searchNames) where

import Data.List.Extra
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as Vector
import Data.Typeable

import Input.Type
import General.Util
import General.Store


data Names = Names deriving Typeable


writeNames :: StoreOut -> [(Maybe Id, Item)] -> IO ()
writeNames store xs = do
    let (ids, strs) = unzip [(i, [' ' | isUName name] ++ lower name) | (Just i, x) <- xs, name <- toName x]
    writeStoreType store Names $ do
        writeStoreV store $ Vector.fromList ids
        writeStoreBS store $ BS.unlines $ map BS.pack strs

toName :: Item -> [String]
toName (IKeyword x) = [x]
toName (IPackage x) = [x]
toName (IModule x) = [last $ splitOn "." x]
toName (IDecl x) = declNames x

searchNames :: StoreIn -> Bool -> [String] -> IO [(Score, Id)]
searchNames store exact xs = do
    let [v,bs] = readStoreList $ readStoreType Names store
    return $ mapMaybe (match exact xs) $ zip (Vector.toList $ readStoreV v) (BS.lines $ readStoreBS bs)

match :: Bool -> [String] -> (Id, BS.ByteString) -> Maybe (Score, Id)
match exact xs = \(ident, str) -> fmap (,ident) $ case () of
    _ | BS.length str < mn -> Nothing
      | not $ all (`BS.isInfixOf` str) xsMatch -> Nothing
      | any (== str) xsPerfect -> Just 0
      | exact -> Nothing
      | any (== str) xsGood -> Just 1
      | any (`BS.isPrefixOf` str) xsPerfect -> Just 2
      | any (`BS.isPrefixOf` str) xsGood -> Just 3
      | otherwise -> Just 4
    where
        mn = sum $ map BS.length xsMatch
        xsMatch = map (BS.pack . lower) xs
        xsPerfect = [BS.pack $ [' ' | isUName x] ++ lower x | x <- xs]
        xsGood = [BS.pack $ [' ' | not $ isUName x] ++ lower x | x <- xs]
