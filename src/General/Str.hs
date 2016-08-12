{-# LANGUAGE PatternGuards #-}

-- | ByteString wrappers which don't require special imports and are all UTF8 safe
module General.Str(
    Str, strPack, strUnpack, strReadFile, strSplitInfix, strNull, strStripPrefix, strTrimStart,
    LStr, lstrPack, lstrUnpack, lstrToChunks, lstrFromChunks,
    Str0, join0, split0
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUS
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

strStripPrefix :: Str -> Str -> Maybe Str
strStripPrefix needle x
    | BS.isPrefixOf needle x = Just $ BS.drop (BS.length needle) x
    | otherwise = Nothing

strTrimStart :: Str -> Str
strTrimStart = BS.dropWhile isSpace

lstrToChunks :: LStr -> [Str]
lstrToChunks = LBS.toChunks

lstrFromChunks :: [Str] -> LStr
lstrFromChunks = LBS.fromChunks

lstrUnpack :: LStr -> String
lstrUnpack = LUS.toString

lstrPack :: String -> LStr
lstrPack = LUS.fromString


type Str0 = Str

join0 :: [String] -> Str0
join0 = BS.pack . intercalate "\0"

split0 :: Str0 -> [Str]
split0 = BS.split '\0'
