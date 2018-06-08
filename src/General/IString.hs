{-# LANGUAGE PatternGuards, DeriveDataTypeable, ViewPatterns, BangPatterns #-}

-- | Interned strings
module General.IString(
    IString, fromIString, toIString
    ) where

import Data.Data
import Data.IORef
import Control.DeepSeq
import General.Str
import qualified Data.Map as Map
import System.IO.Unsafe


data IString = IString {-# UNPACK #-} !Int !Str
    deriving (Data,Typeable)

instance Eq IString where
    IString x _ == IString y _ = x == y

instance Ord IString where
    compare (IString x1 x2) (IString y1 y2)
        | x1 == y1 = EQ
        | otherwise = compare x2 y2

instance Show IString where show = strUnpack . fromIString
instance NFData IString where rnf IString{} = () -- we force the string at construction time


{-# NOINLINE istrings #-}
istrings :: IORef (Map.Map Str IString)
istrings = unsafePerformIO $ newIORef Map.empty

fromIString :: IString -> Str
fromIString (IString _ x) = x

toIString :: Str -> IString
toIString x = unsafePerformIO $ atomicModifyIORef' istrings $ \mp -> case Map.lookup x mp of
    Just v -> (mp, v)
    Nothing -> let res = IString (Map.size mp) x in (Map.insert x res mp, res)
