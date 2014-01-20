
module General.FMIndex(
    FMIndex,
    create, fromHandle,
    extract,
    Find(..), count, locate
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Control.Applicative
import Control.Arrow
import System.IO


data FMIndex a = FMIndex Char [(BS.ByteString, a)]

instance Functor FMIndex where
    fmap f (FMIndex a b) = FMIndex a $ map (second f) b

instance Binary a => Binary (FMIndex a) where
    put (FMIndex a b) = put a >> put b
    get = FMIndex <$> get <*> get

-- assign these indicies to this information
create :: Char -> [(BS.ByteString, a)] -> FMIndex a
create = FMIndex

extract :: FMIndex a -> [(BS.ByteString, a)]
extract (FMIndex _ x) = x

data Find = Exact | Prefix | Suffix | Infix

count :: FMIndex a -> Find -> BS.ByteString -> Int
count idx mode x = length $ locate idx mode x


locate :: FMIndex a -> Find -> BS.ByteString -> [(a, Int)] -- The int is how many characters you are along this string
locate (FMIndex _ xs) mode x = [(i, p) | (a,i) <- xs, Just p <- [op a]]
    where
        op = case mode of
            Exact -> \a -> if x == a then Just 0 else Nothing
            Prefix -> \a -> if x `BS.isPrefixOf` a then Just 0 else Nothing
            Suffix -> \a -> if x `BS.isSuffixOf` a then Just $ BS.length a - BS.length x else Nothing
            Infix -> \a -> let (y,z) = BS.breakSubstring x a in if BS.null z then Nothing else Just $ BS.length y


fromHandle :: Binary a => Handle -> IO (FMIndex a)
fromHandle = fmap decode . LBS.hGetContents
