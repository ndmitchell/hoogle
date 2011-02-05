
module Hoogle.DataBase.SubstrSearch
    (SubstrSearch, createSubstrSearch
    ,searchSubstrSearch
    ,completionsSubstrSearch
    ) where

import Hoogle.Store.All
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import qualified Data.Set as Set
import General.Base
import Hoogle.Type.All
import Hoogle.Score.All

{-
Format 2:

-- build a Huffman table
huffman :: Eq a => [a] -> Huffman a

-- encode a value using the table
-- return the first 32 bits of the encoding, and a mask (will be all 1's if more than 32 bits)
encode :: Huffman a -> [a] -> (Word32, Word32)


-- We have 4 buckets, one per priority level - Prelude first, then base, then platform, then anything
data Substr a = Substr [Bucket a]

-- Each bucket contains the encoding of each entry (a pointer to it) along
-- with the Word32 prefix of each string
-- the 31'st bit is 1 if the string comes from the start of a string
-- and the 32'nd bit is 1 if the string contains upper case letters
-- within each entry, the tree is used to find shifts
-- items are sorted by prefixes
data Bucket a = Bucket {answers :: [a], prefixes :: [Word32], tree :: Tree}

-- at each tree point the range is the start/end index where you may find things with that prefix
-- if the Maybe is Just then all the points in that range are shifted by one bit
data Tree = Tree {range :: (Int, Int), rest :: Maybe (Tree, Tree)}
-}



-- idea for speed improvement
-- store as one long bytestring with \0 between the words, then do findSubstrings to find the indexes
-- store the lengths in a separate bytestring then use index to step through them, retrieving the data as Word8 via foldl
-- store the links in another bytestring with the lengths, but only unpack them when they are needed
-- can even make length==0 code for it's the same string as before, to compress it and reduce searching
-- was previously ~ 0.047 seconds

{-
Description:

Data is stored flattened. For default we expect ~200Kb of disk usage.
-}

-- keys are sorted after being made lower case
data SubstrSearch a = SubstrSearch
    {text :: BS.ByteString -- all the bytestrings, in preference order
    ,lens :: BS.ByteString -- a list of lengths
    ,inds :: Int -> a -- a way of retrieving each index
    }


-- | Create a substring search index. Values are returned in order where possible.
createSubstrSearch :: [(String,a)] -> SubstrSearch a
createSubstrSearch xs = SubstrSearch
    (BSC.pack $ concat ts2)
    (BS.pack $ map fromIntegral ls2)
    (is !!)
    where
        (ts,is) = unzip $ map (first $ map toLower) xs
        (ts2,ls2) = f "" ts

        f x (y:ys) = first (y:) $ second (length y:) $ f y ys
        f x [] = ([],[])


data S a = S
    {sCount :: !Int -- which one are we on
    ,sFocus :: !BS.ByteString -- where we are in the string
    ,sPrefix :: ![(a,EntryView,Score)] -- the prefixes
    ,sInfix :: ![(a,EntryView,Score)] -- the infixes
    }


searchSubstrSearch :: SubstrSearch a -> String -> [(a, EntryView, Score)]
searchSubstrSearch x y = reverse (sPrefix sN) ++ reverse (sInfix sN)
    where
        view = FocusOn y
        match = bsMatch (BSC.pack $ map toLower y)
        sN = BS.foldl f s0 $ lens x
        s0 = S 0 (text x) [] []

        f s ii = addCount $ moveFocus i $ maybe id addMatch t s
            where t = match i $ BS.unsafeTake i $ sFocus s
                  i = fromIntegral ii

        addCount s = s{sCount=sCount s+1}
        moveFocus i s = s{sFocus=BS.unsafeDrop i $ sFocus s}
        addMatch MatchSubstr s = s{sInfix =(inds x $ sCount s,view,textScore MatchSubstr):sInfix s}
        addMatch t s = s{sPrefix=(inds x $ sCount s,view,textScore t):sPrefix s}


data S2 = S2
    {_s2Focus :: !BS.ByteString -- where we are in the string
    ,s2Result :: Set.Set BS.ByteString
    }

completionsSubstrSearch :: SubstrSearch a -> String -> [String]
completionsSubstrSearch x y = map (\x -> y ++ drop ny (BSC.unpack x)) $ take 10 $ Set.toAscList $
                              s2Result $ BS.foldl f (S2 (text x) Set.empty) $ lens x
    where
        ny = length y
        ly = BSC.pack $ map toLower y
        f (S2 foc res) ii = S2 (BS.unsafeDrop i foc) (if ly `BS.isPrefixOf` x then Set.insert x res else res)
            where x = BS.unsafeTake i foc
                  i = fromIntegral ii


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
