{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}

module Output.Tags(Tags, writeTags, readTags, listTags, filterTags, searchTags) where

import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Char8 as BS
import Data.Word

import Input.Type
import Query
import General.Util
import General.Store

-- matches (a,b) if i >= a && i <= b

data Tags = Tags
    {packageNames :: BS.ByteString -- sorted
    ,categoryNames :: BS.ByteString -- sorted
    ,packageIds :: V.Vector (Id, Id)
    ,categoryOffsets :: V.Vector Word32 -- position I start, use +1 to find one after I end
    ,categoryIds :: V.Vector (Id, Id)
    ,moduleNames :: BS.ByteString -- not sorted
    ,moduleIds :: V.Vector (Id, Id)
    ,completionNames :: BS.ByteString -- things I want to complete to
    } deriving Typeable

join0 :: [String] -> BS.ByteString
join0 = BS.pack . intercalate "\0"

split0 :: BS.ByteString -> [BS.ByteString]
split0 = BS.split '\0'

writeTags :: StoreWrite -> (String -> Bool) -> (String -> [(String,String)]) -> [(Maybe Id, Item)] -> IO ()
writeTags store keep extra xs = storeWriteType store (undefined :: Tags) $ do
    let splitPkg = splitIPackage xs
    let packagesRaw = addRange splitPkg
    let packages = sortOn (lower . fst) packagesRaw
    let categories = map (first snd . second reverse) $ Map.toList $ Map.fromListWith (++)
            [(((weightTag ex, both lower ex), joinPair ":" ex),[rng]) | (p,rng) <- packagesRaw, ex <- extra p]

    storeWriteBS store $ join0 $ map fst packages
    storeWriteBS store $ join0 $ map fst categories
    storeWriteV store $ V.fromList $ map snd packages
    storeWriteV store $ V.fromList $ scanl (+) (0 :: Word32) $ map (genericLength . snd) categories
    storeWriteV store $ V.fromList $ concatMap snd categories

    let modules = addRange $ concatMap (splitIModule . snd) splitPkg
    storeWriteBS store $ join0 $ map (lower . fst) modules
    storeWriteV store $ V.fromList $ map snd modules

    storeWriteBS store $ join0 $
        takeWhile ("set:" `isPrefixOf`) (map fst categories) ++
        map ("package:"++) (filter keep $ map fst packages) ++
        map (joinPair ":") (sortOn (weightTag &&& both lower) $ nubOrd [ex | (p,_) <- packages, keep p, ex <- extra p])
    where
        addRange :: [(String, [(Maybe Id,a)])] -> [(String, (Id, Id))]
        addRange xs = [(a, (minimum is, maximum is)) | (a,b) <- xs, let is = mapMaybe fst b, a /= "", is /= []]

        weightTag ("set",x) = fromMaybe 0.9 $ lookup x [("stackage",0.0),("haskell-platform",0.1)]
        weightTag ("package",x) = 1
        weightTag ("category",x) = 2
        weightTag ("license",x) = 3
        weightTag _ = 4


readTags :: StoreRead -> Tags
readTags store = Tags (storeReadBS x1) (storeReadBS x2) (storeReadV x3) (storeReadV x4) (storeReadV x5) (storeReadBS x6) (storeReadV x7) (storeReadBS x8)
    where [x1,x2,x3,x4,x5,x6,x7,x8] = storeReadList $ storeReadType (undefined :: Tags) store


listTags :: Tags -> [String]
listTags Tags{..} = map BS.unpack $ split0 completionNames

lookupTag :: Tags -> (String, String) -> [(Id,Id)]
lookupTag Tags{..} ("is",'p':xs) | xs `isPrefixOf` "ackage" = map (dupe . fst) $ V.toList packageIds
lookupTag Tags{..} ("is",'m':xs) | xs `isPrefixOf` "odule" = map (dupe . fst) $ V.toList moduleIds
lookupTag Tags{..} ("package",x) = map (packageIds V.!) $ findIndices (== BS.pack x) $ split0 packageNames
lookupTag Tags{..} ("module",lower -> x) = map (moduleIds V.!) $ findIndices f $ split0 moduleNames
    where
        f | Just x <- stripPrefix "." x, Just x <- stripSuffix "." x = (==) (BS.pack x)
          | Just x <- stripPrefix "." x = BS.isPrefixOf $ BS.pack x
          | otherwise = let y = BS.pack x; y2 = BS.pack $ ('.':x)
                        in \v -> y `BS.isPrefixOf` v || y2 `BS.isInfixOf` v
lookupTag Tags{..} x = concat
    [ V.toList $ V.take (fromIntegral $ end - start) $ V.drop (fromIntegral start) categoryIds
    | i <- findIndices (== BS.pack (joinPair ":" x)) $ split0 categoryNames
    , let start = categoryOffsets V.! i, let end = categoryOffsets V.! (i + 1)
    ]

filterTags :: Tags -> [Query] -> (Id -> Bool)
filterTags ts qs = let fs = map (filterTags2 ts . snd) $ groupSort $ map (scopeCategory &&& id) $ filter isQueryScope qs in \i -> all ($ i) fs

filterTags2 ts qs = \i -> not (negq i) && (null pos || posq i)
    where (posq,negq) = both inRanges (pos,neg)
          (pos, neg) = both (map snd) $ partition fst $ concatMap f qs
          f (QueryScope sense cat val) = map (sense,) $ lookupTag ts (cat,val)

searchTags :: Tags -> [Query] -> [Id]
searchTags ts [] = map fst $ V.toList $ packageIds ts
searchTags ts qs = if null xs then x else filter (`Set.member` foldl1' Set.intersection (map Set.fromList xs)) x
    where x:xs = [map fst $ lookupTag ts (cat,val) | QueryScope True cat val <- qs]
