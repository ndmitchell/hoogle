{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hoogle.DataBase2.Type where

import Data.Binary
import qualified Data.ByteString.Char8 as BS
import Foreign.Storable

newtype Pos = Pos Word32 deriving (Binary,Eq,Ord,Num,Show,Storable)

newtype Package = Package BS.ByteString deriving (Binary,Ord,Eq,Show)


newPackage :: String -> Package
newPackage = Package . BS.pack
