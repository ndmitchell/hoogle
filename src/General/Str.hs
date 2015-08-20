{-# LANGUAGE PatternGuards, BangPatterns #-}

-- | ByteString wrappers which don't require special imports and are all UTF8 safe 
module General.Str(
    Str, strPack, strUnpack, strReadFile, strSplitInfix, strNull, strConcat, strStripPrefix, strStripSuffix, strTrimStart, strUnlines, strUnwords,
    LStr, lstrPack, lstrUnpack, lstrLines, lstrToChunks, lstrFromChunks, lstrToStr,
    Str0, join0, split0,
    general_str_test
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUS
import qualified Data.ByteString.Internal as IBS
import System.IO.Unsafe
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Data.Bits
import General.Util
import Test.QuickCheck(quickCheck)
import Data.IORef
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
lstrPack :: String -> LStr
lstrPack [] = LBS.empty
lstrPack xs = unsafePerformIO $ do
    ref <- newIORef undefined
    fmap LBS.fromChunks $ flip unfoldrIO xs $ \xs -> do
        if null xs then return Nothing else do
            bs <- IBS.create 1024 $ \ptr -> do
                (ptr2, xs2) <- f ptr (ptr `plusPtr` 1020) xs
                writeIORef ref (ptr2 `minusPtr` ptr, xs2)
            (n,xs) <- readIORef ref
            return $ Just (BS.unsafeTake n bs, xs)
    where
        f :: Ptr Word8 -> Ptr Word8 -> String -> IO (Ptr Word8, String)
        f !ptr !end xs | null xs || ptr >= end = return (ptr, xs)
        f !ptr !end (x:xs)
            | x <= '\x7f' = poke ptr (IBS.c2w x) >> f (plusPtr ptr 1) end xs
            | otherwise = case ord x of
                oc | oc <= 0x7ff -> do
                        poke ptr $ fromIntegral $ 0xc0 + (oc `shiftR` 6)
                        pokeByteOff ptr 1 (fromIntegral $ 0x80 + oc .&. 0x3f :: Word8)
                        f (plusPtr ptr 2) end xs
                   | oc <= 0xffff -> do
                        poke ptr $ fromIntegral $ 0xe0 + (oc `shiftR` 12)
                        pokeByteOff ptr 1 (fromIntegral $ 0x80 + ((oc `shiftR` 6) .&. 0x3f) :: Word8)
                        pokeByteOff ptr 2 (fromIntegral $ 0x80 + oc .&. 0x3f :: Word8)
                        f (plusPtr ptr 3) end xs
                   | otherwise -> do
                        poke ptr $ fromIntegral $ 0xf0 + (oc `shiftR` 18)
                        pokeByteOff ptr 1 (fromIntegral $ 0x80 + ((oc `shiftR` 12) .&. 0x3f) :: Word8)
                        pokeByteOff ptr 2 (fromIntegral $ 0x80 + ((oc `shiftR` 6) .&. 0x3f) :: Word8)
                        pokeByteOff ptr 3 (fromIntegral $ 0x80 + oc .&. 0x3f :: Word8)
                        f (plusPtr ptr 4) end xs


unfoldrIO :: (a -> IO (Maybe (b,a))) -> a -> IO [b]
unfoldrIO f = go
    where go z = do
            x <- f z
            case x of
                Nothing -> return []
                Just (x, z) -> do
                        xs <- unsafeInterleaveIO $ go z
                        return $ x : xs


type Str0 = Str

join0 :: [String] -> Str0
join0 = BS.pack . intercalate "\0"

split0 :: Str0 -> [Str]
split0 = BS.split '\0'


general_str_test :: IO ()
general_str_test = do
    testing_ "General.Str.lstrPack" $ do
        quickCheck $ \x -> lstrPack x == LUS.fromString x
