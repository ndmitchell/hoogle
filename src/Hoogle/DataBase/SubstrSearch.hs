{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.DataBase.SubstrSearch
    (SubstrSearch, createSubstrSearch
    ,searchSubstrSearch
    ,searchExactSearch
    ,completionsSubstrSearch
    ) where

import Hoogle.Store.All
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as C
import General.Base
import Data.Array
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
    {text :: BString  -- all the bytestrings, in preference order
    ,lens :: BString  -- a list of lengths
    ,inds :: Array Int a  -- the results
    }
    deriving Typeable

instance NFData a => NFData (SubstrSearch a) where
    rnf (SubstrSearch a b c) = rnf (a `seq` (),b `seq` (),c)

-- | Create a substring search index. Values are returned in order where possible.
createSubstrSearch :: [(String,a)] -> SubstrSearch a
createSubstrSearch xs = SubstrSearch
    (fromString $ concat ts2)
    (BS.pack $ map fromIntegral ls2)
    (listArray (0,length is-1) is)
    where
        (ts,is) = unzip xs
        (ts2,ls2) = f "" ts

        f x (y:ys) = first (y:) $ second (length y:) $ f y ys
        f x [] = ([],[])


data S a = S
    {sCount :: !Int -- which one are we on
    ,sFocus :: !BS.ByteString -- where we are in the string
    ,sPrefix :: ![(a,EntryView,Score)] -- the prefixes
    ,sInfix :: ![(a,EntryView,Score)] -- the infixes
    }


toChar :: Word8 -> Char
toChar = C.chr . fromIntegral

-- | Unsafe version of 'fromChar'
ascii :: Char -> Word8
ascii = fromIntegral . C.ord
{-# INLINE ascii #-}

searchSubstrSearch :: SubstrSearch a -> String -> [(a, EntryView, Score)]
searchSubstrSearch x y = reverse (sPrefix sN) ++ reverse (sInfix sN)
    where
        view = FocusOn y
        match = bsMatch (BSC.pack y)
        sN = BS.foldl f s0 $ lens x
        s0 = S 0 (text x) [] []

        f s ii = addCount $ moveFocus i $ maybe id addMatch t s
            where t = match i $ BS.map (ascii . toChar)
                      $ BS.unsafeTake i $ sFocus s
                  i = fromIntegral ii

        addCount s = s{sCount=sCount s+1}
        moveFocus i s = s{sFocus=BS.unsafeDrop i $ sFocus s}
        addMatch MatchSubstr s = s{sInfix =(inds x ! sCount s,view,textScore MatchSubstr):sInfix s}
        addMatch t s = s{sPrefix=(inds x ! sCount s,view,textScore t):sPrefix s}

searchExactSearch :: SubstrSearch a -> String -> [(a, EntryView, Score)]
searchExactSearch x y = reverse (sPrefix sN)
    where
        view = FocusOn y
        match = bsMatch (BSC.pack y)
        sN = BS.foldl f s0 $ lens x
        s0 = S 0 (text x) [] []

        f s ii = addCount $ moveFocus i $ maybe id addMatch t s
            where t = match i $ BS.unsafeTake i $ sFocus s
                  i = fromIntegral ii

        addCount s = s{sCount=sCount s+1}
        moveFocus i s = s{sFocus=BS.unsafeDrop i $ sFocus s}
        addMatch MatchExact s = s{sPrefix=(inds x ! sCount s,view,textScore MatchExact):sPrefix s}
        addMatch _ s = s


data S2 = S2
    {_s2Focus :: !BS.ByteString -- where we are in the string
    ,s2Result :: Set.Set BS.ByteString
    }

completionsSubstrSearch :: SubstrSearch a -> String -> [String]
completionsSubstrSearch x y = map (\x -> y ++ drop ny (BSC.unpack x)) $ take 10 $ Set.toAscList $
                              s2Result $ BS.foldl f (S2 (text x) Set.empty) $ lens x
    where
        ny = length y
        ly = fromString $ map toLower y
        f (S2 foc res) ii = S2 (BS.unsafeDrop i foc) (if ly `BS.isPrefixOf` x then Set.insert x res else res)
            where x = BS.map (ascii . toLower . toChar) $ BS.unsafeTake i foc
                  i = fromIntegral ii


instance Show a => Show (SubstrSearch a) where
    show x = "SubstrSearch"

instance (Typeable a, Store a) => Store (SubstrSearch a) where
    put (SubstrSearch a b c) = putDefer $ put3 a b c
    get = getDefer $ get3 SubstrSearch


-- if first word is empty, always return Exact/Prefix
-- if first word is a single letter, do elemIndex
-- if first word is multiple, do isPrefixOf's but only up until n from the end
-- partially apply on the first word
bsMatch :: BS.ByteString -> Int -> BS.ByteString -> Maybe TextMatch
bsMatch x
    | nx == 0 = \ny _ -> Just $ if ny == 0 then MatchExact else MatchPrefix
    | nx == 1 = \ny y ->
                maybe (bsCharMatch MatchExactCI MatchPrefixCI False
                           (BS.head (bsLower x)) ny (bsLower y))
                    Just (bsCharMatch MatchExact MatchPrefix True
                              (BS.head x) ny y)
    | otherwise = \ny y ->
        maybe (bsWordMatch MatchExactCI MatchPrefixCI False
                   (bsLower x) ny (bsLower y))
            Just (bsWordMatch MatchExact MatchPrefix True x ny y)
 where
    nx = BS.length x

    bsLower = BS.map (ascii . toLower . toChar)

    bsCharMatch exactKind prefixKind ignoreSubstr c ny y =
        case BS.elemIndex c y of
            Nothing -> Nothing
            Just 0 -> Just $ if ny == 1
                             then exactKind
                             else prefixKind
            Just _
                | ignoreSubstr -> Nothing
                | otherwise    -> Just MatchSubstr

    bsWordMatch exactKind prefixKind ignoreSubstr x' ny y =
        if BS.isPrefixOf x' y
        then Just (if nx == ny then exactKind else prefixKind)
        else if not ignoreSubstr && BS.isInfixOf x' y
             then Just MatchSubstr
             else Nothing