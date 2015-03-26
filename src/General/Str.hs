{-# LANGUAGE PatternGuards #-}

module General.Str(
    Str, strPack, strUnpack, strReadFile, strSplitInfix,
    LStr, lstrPack, lstrUnpack, lstrToChunks, lstrFromChunks
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUS


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


lstrToChunks :: LStr -> [Str]
lstrToChunks = LBS.toChunks

lstrFromChunks :: [Str] -> LStr
lstrFromChunks = LBS.fromChunks

lstrUnpack :: LStr -> String
lstrUnpack = LUS.toString

lstrPack :: String -> LStr
lstrPack = LUS.fromString
