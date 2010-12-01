{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.ParseError where

import Data.Data

-- | 1 based
data ParseError = ParseError {lineNo :: Int, columnNo :: Int, parseError :: String}
                  deriving (Ord,Eq,Data,Typeable)

emptyParseError :: ParseError
emptyParseError = ParseError 0 0 ""

instance Show ParseError where
    show (ParseError line col err) = "Parse error " ++ show line ++ ":" ++ show col ++ ": " ++ err
