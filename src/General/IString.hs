{-# LANGUAGE PatternGuards, DeriveDataTypeable, ViewPatterns, BangPatterns #-}

-- | Interned strings
module General.IString(
    IString, fromIString, toIString
    ) where

import Data.Data
import Data.IORef
import Control.DeepSeq
import Data.String
import qualified Foundation as Fdn
import qualified Data.Map as Map
import System.IO.Unsafe


data IString = IString {-# UNPACK #-} !Int !Fdn.String String
    deriving (Data,Typeable)

instance Eq IString where
    IString x _ _ == IString y _ _ = x == y

instance Ord IString where
    compare (IString x1 x2 _) (IString y1 y2 _)
        | x1 == y1 = EQ
        | otherwise = compare x2 y2

instance Show IString where show = fromIString
instance Read IString where readsPrec _ x = [(toIString x,"")]
instance IsString IString where fromString = toIString
instance NFData IString where rnf (IString _ _ _) = () -- we force the string at construction time

{-# NOINLINE istrings #-}
istrings :: IORef (Map.Map Fdn.String IString)
istrings = unsafePerformIO $ newIORef Map.empty

fromIString :: IString -> String
fromIString (IString _ _ x) = x

toIString :: String -> IString
toIString (fromString -> !x) = unsafePerformIO $ atomicModifyIORef istrings $ \mp -> case Map.lookup x mp of
    Just v -> (mp, v)
    Nothing -> let res = IString (Map.size mp) x (Fdn.toList x) in (Map.insert x res mp, res)
