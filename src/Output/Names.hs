{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable #-}

module Output.Names(writeNames, searchNames) where

import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

import Input.Type
import General.Util
import General.Store


data Names = Names deriving Typeable


writeNames :: StoreOut -> [(Maybe Id, Item)] -> IO ()
writeNames store xs = writeStoreType store Names $ writeStoreBS store $ BS.pack $ unlines
    [show i ++ " " ++ [' ' | isUName name] ++ lower name | (Just i, x) <- xs, name <- toName x]

toName :: Item -> [String]
toName (IKeyword x) = [x]
toName (IPackage x) = [x]
toName (IModule x) = [last $ splitOn "." x]
toName (IDecl x) = declNames x

searchNames :: StoreIn -> Bool -> [String] -> IO [(Score, Id)]
searchNames store exact xs = do
    let src = BS.lines $ readStoreBS $ readStoreType Names store
    return $ mapMaybe (match exact xs) src

match :: Bool -> [String] -> BS.ByteString -> Maybe (Score, Id)
match exact xs = \line ->
    let (ident, str) = second (BS.drop 1) $ BS.break (== ' ') line
        ident2 = read $ BS.unpack ident
    in fmap (,ident2) $ case () of
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
