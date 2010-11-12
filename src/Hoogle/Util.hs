{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Util where

import Data.Data


-- | 1 based
data ParseError = ParseError {lineNo :: Int, columnNo :: Int, parseError :: String}
                  deriving (Ord,Eq,Data,Typeable)

instance Show ParseError where
    show (ParseError line col err) = "Parse error " ++ show line ++ ":" ++ show col ++ ": " ++ err
