{-# LANGUAGE PatternGuards, BangPatterns #-}

-- | ByteString wrappers which don't require special imports and are all UTF8 safe 
module General.Str(
    Str, strPack, strUnpack, strReadFile, strSplitInfix, strNull, strConcat, strStripPrefix, strStripSuffix, strTrimStart, strUnlines, strUnwords,
    LStr, lstrPack, lstrUnpack, lstrLines, lstrToChunks, lstrFromChunks, lstrToStr,
    Str0, join0, split0,
    general_str_test
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUS
import System.IO.Unsafe
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception
import Foreign.Storable
import Data.Bits
import General.Util
import Test.QuickCheck(quickCheck)
import Data.Char
import Data.List


type Str = BS.ByteString

type LStr = LBS.ByteString


strPack :: String -> Str
strPack = US.fromString

strUnpack :: Str -> String
strUnpack = US.toString

strReadFile :: FilePath -> IO Str
strReadFile = BS.readFile

strSplitInfix :: Str -> Str -> Maybe (Str, Str)
strSplitInfix needle haystack
    | (a,b) <- BS.breakSubstring needle haystack
    , not $ BS.null b
    = Just (a, BS.drop (BS.length needle) b)
strSplitInfix _ _ = Nothing

strNull :: Str -> Bool
strNull = BS.null

strConcat :: [Str] -> Str
strConcat = BS.concat

strStripPrefix :: Str -> Str -> Maybe Str
strStripPrefix needle x
    | BS.isPrefixOf needle x = Just $ BS.drop (BS.length needle) x
    | otherwise = Nothing

strStripSuffix :: Str -> Str -> Maybe Str
strStripSuffix needle x
    | BS.isSuffixOf needle x = Just $ BS.take (BS.length x - BS.length needle) x
    | otherwise = Nothing

strTrimStart :: Str -> Str
strTrimStart = BS.dropWhile isSpace

strUnlines :: [Str] -> Str
strUnlines = BS.unlines

strUnwords :: [Str] -> Str
strUnwords = BS.unwords

lstrLines :: LStr -> [LStr]
lstrLines = LBS.lines

lstrToChunks :: LStr -> [Str]
lstrToChunks = LBS.toChunks

lstrToStr :: LStr -> Str
lstrToStr = LBS.toStrict

lstrFromChunks :: [Str] -> LStr
lstrFromChunks = LBS.fromChunks

lstrUnpack :: LStr -> String
lstrUnpack = LUS.toString

-- | Significantly faster than the utf8-string one, and also lazy as well
-- | Converts a Haskell string into a UTF8 encoded bytestring.
lstrPack :: String -> B.ByteString
lstrPack [] = B.empty
lstrPack xs = packChunks 32 xs
  where
    packChunks n xs = case packUptoLenBytes n xs of
        (bs, []) -> B.chunk bs B.Empty
        (bs, xs) -> B.Chunk bs (packChunks (min (n * 2) B.smallChunkSize) xs)

    packUptoLenBytes :: Int -> String -> (S.ByteString, String)
    packUptoLenBytes len xs = unsafeCreateUptoN' len $ \ptr -> do
        (end, xs) <- go ptr (ptr `plusPtr` (len-4)) xs
        return (end `minusPtr` ptr, xs)

    -- end is the last position at which you can write a whole 4 byte sequence safely
    go :: Ptr Word8 -> Ptr Word8 -> String -> IO (Ptr Word8, String)
    go !ptr !end xs | ptr > end = return (ptr, xs)
    go !ptr !_   [] = return (ptr, [])
    go !ptr !end (x:xs)
        | x <= '\x7f' = poke ptr (S.c2w x) >> go (plusPtr ptr 1) end xs
        | otherwise = case ord x of
            oc | oc <= 0x7ff -> do
                    poke ptr $ fromIntegral $ 0xc0 + (oc `shiftR` 6)
                    pokeElemOff ptr 1 $ fromIntegral $ 0x80 + oc .&. 0x3f
                    go (plusPtr ptr 2) end xs
               | oc <= 0xffff -> do
                    poke ptr $ fromIntegral $ 0xe0 + (oc `shiftR` 12)
                    pokeElemOff ptr 1 $ fromIntegral $ 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                    pokeElemOff ptr 2 $ fromIntegral $ 0x80 + oc .&. 0x3f
                    go (plusPtr ptr 3) end xs
               | otherwise -> do
                    poke ptr $ fromIntegral $ 0xf0 + (oc `shiftR` 18)
                    pokeElemOff ptr 1 $ fromIntegral $ 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                    pokeElemOff ptr 2 $ fromIntegral $ 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                    pokeElemOff ptr 3 $ fromIntegral $ 0x80 + oc .&. 0x3f
                    go (plusPtr ptr 4) end xs


type Str0 = Str

join0 :: [String] -> Str0
join0 = BS.pack . intercalate "\0"

split0 :: Str0 -> [Str]
split0 = BS.split '\0'


general_str_test :: IO ()
general_str_test = do
    testing_ "General.Str.lstrPack" $ do
        quickCheck $ \x -> lstrPack x == LUS.fromString x


---------------------------------------------------------------------
-- COPIED FROM BYTESTRING
-- These functions are copied verbatum from Data.ByteString.Internal
-- I suspect their lack of export is an oversight

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (S.ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

-- | Create ByteString of up to size @l@ and use action @f@ to fill it's contents which returns its true size.
createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (S.ByteString, a)
createUptoN' l f = do
    fp <- S.mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return (S.PS fp 0 l', res)
{-# INLINE createUptoN' #-}
