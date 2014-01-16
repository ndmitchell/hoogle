
module General.FMIndex(
    create, fromHandle,
    extract,
    Find(..), count, locate
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Control.Applicative
import System.IO


data FMIndex = FMIndex Char [(BS.ByteString, Int)]

instance Binary FMIndex where
    put (FMIndex a b) = put a >> put b
    get = FMIndex <$> get <*> get

-- assign these indicies to this information
create :: Char -> [(BS.ByteString, Int)] -> FMIndex
create = FMIndex

extract :: FMIndex -> [(BS.ByteString, Int)]
extract (FMIndex _ x) = x

data Find = Exact | Prefix | Suffix | Infix

count :: FMIndex -> Find -> BS.ByteString -> Int
count idx mode x = length $ locate idx mode x


locate :: FMIndex -> Find -> BS.ByteString -> [(Int, Int)] -- The int for this string, the int for the next string, the position between them
locate (FMIndex _ xs) mode x = [(i, p) | (a,i) <- xs, Just p <- [op a]]
    where
        op = case mode of
            Exact -> \a -> if x == a then Just 0 else Nothing
            Prefix -> \a -> if x `BS.isPrefixOf` a then Just 0 else Nothing
            Suffix -> \a -> if x `BS.isSuffixOf` a then Just $ BS.length a - BS.length x else Nothing
            Infix -> \a -> let (y,z) = BS.breakSubstring x a in if BS.null z then Nothing else Just $ BS.length y


fromHandle :: Handle -> IO FMIndex
fromHandle = fmap decode . LBS.hGetContents
