{-# LANGUAGE FlexibleContexts, PatternGuards #-}

-- | Burrows-Wheeler Transform, based on <http://www.cs.jhu.edu/~langmea/resources/bwt_fm.pdf>.
module General.BurrowsWheeler(compress, compressIndicies, decompress) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Algorithms.AmericanFlag as AmericanFlag
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST
import Data.Word
import Data.Function


compress :: BS.ByteString -> (Int, BS.ByteString)
compress bs = (i, fst $ BS.unfoldrN (BS.length bs) step 0)
    where (i, vs) = compressIndicies bs
          step i = Just (BS.index bs (fromIntegral $ vs V.! i-1), i + 1)


compressIndicies :: BS.ByteString -> (Int, V.Vector Word32)
compressIndicies bs | Just i <- V.elemIndex 0 is = (i, V.take i is V.++ V.drop (i+1) is)
    where
        len = BS.length bs
        is = V.modify (AmericanFlag.sortBy (compare `on` slice) terminate size index) $
                    V.enumFromN (0 :: Word32) $ len + 1

        slice i = BS.drop (fromIntegral i) bs

        -- Copied from the Lexicographic ByteString instance, adapted to use an offset
        size = 257
        terminate b i = i + fromIntegral b >= len
        index i b
            | i + fromIntegral b >= len = 0
            | otherwise = fromIntegral (BS.index bs $ i + fromIntegral b) + 1


decompress :: (Int, BS.ByteString) -> BS.ByteString
decompress (pos, bs) = BS.reverse $ fst $ BS.unfoldrN (BS.length bs) step 0
    where
        step i | j == -1 = Nothing
               | otherwise = let c = BS.index bs j in Just (c, fromIntegral $ (first V.! fromIntegral c) + (ranks V.! j) + 1)
            where j = if i >= pos then i-1 else i
        (ranks, tots) = rankBwt bs
        first = firstCol tots


rankBwt :: BS.ByteString -> (V.Vector Word32, V.Vector Word32)
rankBwt bs = runST $ do
    tots <- VM.replicate 256 0
    ranks <- V.generateM (BS.length bs) $ \i -> do
        let c = fromIntegral $ BS.index bs i
        j <- VM.read tots c
        VM.write tots c $ j + 1
        return j
    tots <- V.freeze tots
    return (ranks, tots)


-- If you sorted the input, at what index would 'c' come
firstCol :: V.Vector Word32 -> V.Vector Word32
firstCol = V.prescanl' (+) 0
