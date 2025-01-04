{-# LANGUAGE PatternGuards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | ByteString wrappers which don't require special imports and are all UTF8 safe
module General.Str(
    Str, strPack, strUnpack, strNull, strCopy, strCons,
    BStr, bstrPack, bstrUnpack, bstrReadFile, bstrSplitInfix, bstrNull, bstrStripPrefix, bstrTrimStart,
    LBStr, lbstrPack, lbstrUnpack, lbstrToChunks, lbstrFromChunks,
    BStr0, bstr0Join, bstr0Split
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUS
import qualified Data.Text as T
import Control.DeepSeq
import Data.Char
import Data.Data
import Data.List
import Data.Semigroup
import Data.String
import Prelude


newtype Str = Str { fromStr :: T.Text }
    deriving (Data, Typeable, Eq, Ord, Semigroup, Monoid)

instance Show Str where show = strUnpack
instance NFData Str where rnf x = x `seq` ()


type BStr = BS.ByteString

type LBStr = LBS.ByteString


strPack :: String -> Str
strPack = Str . T.pack

strUnpack :: Str -> String
strUnpack = T.unpack . fromStr

strCons :: Char -> Str -> Str
strCons c = Str . T.cons c . fromStr

strCopy :: Str -> Str
strCopy = Str . T.copy . fromStr

strNull :: Str -> Bool
strNull = T.null . fromStr

bstrPack :: String -> BStr
bstrPack = US.fromString

bstrUnpack :: BStr -> String
bstrUnpack = US.toString

bstrReadFile :: FilePath -> IO BStr
bstrReadFile = BS.readFile

bstrSplitInfix :: BStr -> BStr -> Maybe (BStr, BStr)
bstrSplitInfix needle haystack
    | (a,b) <- BS.breakSubstring needle haystack
    , not $ BS.null b
    = Just (a, BS.drop (BS.length needle) b)
bstrSplitInfix _ _ = Nothing

bstrNull :: BStr -> Bool
bstrNull = BS.null

bstrStripPrefix :: BStr -> BStr -> Maybe BStr
bstrStripPrefix needle x
    | BS.isPrefixOf needle x = Just $ BS.drop (BS.length needle) x
    | otherwise = Nothing

bstrTrimStart :: BStr -> BStr
bstrTrimStart = BS.dropWhile isSpace

lbstrToChunks :: LBStr -> [BStr]
lbstrToChunks = LBS.toChunks

lbstrFromChunks :: [BStr] -> LBStr
lbstrFromChunks = LBS.fromChunks

lbstrUnpack :: LBStr -> String
lbstrUnpack = LUS.toString

lbstrPack :: String -> LBStr
lbstrPack = LUS.fromString


type BStr0 = BStr

bstr0Join :: [String] -> BStr0
bstr0Join = LBS.toStrict . LUS.fromString . intercalate "\0"

bstr0Split :: BStr0 -> [BStr]
bstr0Split = BS.split '\0'
