{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.All(module X, module Hoogle.Type.All) where

import Hoogle.Type.Item           as X
import Hoogle.Type.Documentation  as X
import Hoogle.Type.TypeSig        as X
import Hoogle.Type.Result         as X


import Data.Data

-- | 1 based
data ParseError = ParseError {lineNo :: Int, columnNo :: Int, parseError :: String}
                  deriving (Ord,Eq,Data,Typeable)

emptyParseError = ParseError 0 0 ""

instance Show ParseError where
    show (ParseError line col err) = "Parse error " ++ show line ++ ":" ++ show col ++ ": " ++ err
