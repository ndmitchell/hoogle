
module Hoogle.DataBase.SubstrSearch
    (SubstrSearch, createSubstrSearch
    ,searchSubstrSearch
    ,completionsSubstrSearch
    ) where

import Data.Binary.Defer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import General.Code
import Hoogle.Type.All
import Hoogle.Score.All
import Data.Function


-- idea for speed improvement
-- store as one long bytestring with \0 between the words, then do findSubstrings to find the indexes
-- store the lengths in a separate bytestring then use index to step through them, retrieving the data as Word8 via foldl
-- store the links in another bytestring with the lengths, but only unpack them when they are needed
-- can even make length==0 code for it's the same string as before, to compress it and reduce searching
-- was previously ~ 0.047 seconds

{-
Description:

The substring search is an array of (key,value) pairs. The keys are sorted case
sensitively.

To do a search we binary chop to find the range where the exact prefixes lie,
then we binary chop to find the range where the inexact prefixes lie, then we
scan linearly to find all substrings.

Data is stored flattened. For default we expect ~200Kb of disk usage.
-}

-- keys are sorted after being made lower case
data SubstrSearch a = SubstrSearch
    {text :: BS.ByteString -- all the bytestrings, lowercase, sorted
    ,lens :: BS.ByteString -- a list of lengths, 0 means the string was identical to the previous one
    ,inds :: Int -> a -- a way of retrieving each index
    }


-- | Create a substring search index. Values are returned in order where possible.
createSubstrSearch :: [(String,a)] -> SubstrSearch a
createSubstrSearch xs = SubstrSearch
    (BSC.pack $ concat ts2)
    (BS.pack $ map fromIntegral ls2)
    (is !!)
    where
        (ts,is) = unzip $ sortBy (compare `on` fst) $ map (first $ map toLower) xs
        (ts2,ls2) = f "" ts

        f x (y:ys) | x == y = second (0:) $ f x ys
                   | otherwise = first (y:) $ second (length y:) $ f y ys
        f x [] = ([],[])


data S a = S
    {sCount :: !Int -- which one are we on
    ,sFocus :: !BS.ByteString -- where we are in the string
    ,sLast :: !(Maybe TextMatch) -- the last result
    ,sPrefix :: ![(a,EntryView,Score)] -- the prefixes
    ,sInfix :: ![(a,EntryView,Score)] -- the infixes
    }


searchSubstrSearch :: Eq a => SubstrSearch a -> String -> [(a, EntryView, Score)]
searchSubstrSearch x y = reverse (sPrefix sN) ++ reverse (sInfix sN)
    where
        view = FocusOn y
        match = bsMatch (BSC.pack $ map toLower y)
        sN = BS.foldl f s0 $ lens x
        s0 = S 0 (text x) Nothing [] []

        f s 0 = addCount $ case sLast s of
            Nothing -> s
            Just x -> addMatch x s
        f s ii = addCount $ moveFocus i $ maybe id addMatch t $ s{sLast=t}
            where t = match i $ BS.unsafeTake i $ sFocus s
                  i = fromIntegral ii

        addCount s = s{sCount=sCount s+1}
        moveFocus i s = s{sFocus=BS.unsafeDrop i $ sFocus s}
        addMatch MatchSubstr s = s{sInfix =(inds x $ sCount s,view,textScore MatchSubstr):sInfix s}
        addMatch t s = s{sPrefix=(inds x $ sCount s,view,textScore t):sPrefix s}


completionsSubstrSearch :: SubstrSearch a -> String -> [String]
completionsSubstrSearch _ _ = []


instance Show a => Show (SubstrSearch a) where
    show x = "SubstrSearch"

instance (Bin.Binary a, BinaryDeferGet a, FixedBinary a) => BinaryDefer (SubstrSearch a) where
    put x = putDefer $ putLazyByteString $ Bin.runPut $ putBinary Bin.put x

    get = do
        g <- binaryDeferGet
        x <- getDefer getLazyByteString
        return $ Bin.runGet (getBinary (fixedSize $ tyUnGet g) g) x
        where
            tyUnGet :: Bin.Get a -> a
            tyUnGet = undefined


putBinary :: (a -> Bin.Put) -> SubstrSearch a -> Bin.Put
putBinary p x = do
    Bin.put $ text x
    Bin.put $ lens x
    Bin.put $ fromLBS $ Bin.runPut $ mapM_ (p . inds x) [0.. fromIntegral (BS.length $ lens x) - 1]


getBinary :: Int -> Bin.Get a -> Bin.Get (SubstrSearch a)
getBinary size g = do
    text <- Bin.get
    lens <- Bin.get
    indsData <- Bin.get
    let inds i = Bin.runGet g $ toLBS $ BS.take size $ BS.drop (i * size) indsData
    return $ SubstrSearch text lens inds


fromLBS = BS.concat . LBS.toChunks
toLBS = LBS.fromChunks . return



-- if first word is empty, always return Exact/Prefix
-- if first word is a single letter, do elemIndex
-- if first word is multiple, do isPrefixOf's but only up until n from the end
-- partially apply on the first word
bsMatch :: BS.ByteString -> Int -> BS.ByteString -> Maybe TextMatch
bsMatch x
    | nx == 0 = \ny _ -> Just $ if ny == 0 then MatchExact else MatchPrefix
    | nx == 1 = let c = BS.head x in \ny y -> case BS.elemIndex c y of
        Nothing -> Nothing
        Just 0 -> Just $ if ny == 1 then MatchExact else MatchPrefix
        Just _ -> Just MatchSubstr
    | otherwise = \ny y -> if BS.isPrefixOf x y then Just (if nx == nx then MatchExact else MatchPrefix)
                           else if BS.isInfixOf x y then Just MatchSubstr else Nothing
    where nx = BS.length x
