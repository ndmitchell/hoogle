{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Util where

import qualified Text.ParserCombinators.Parsec as P
import Data.Data


-- | 1 based
data ParseError = ParseError {lineNo :: Int, columnNo :: Int, parseError :: String}
                  deriving (Ord,Eq,Data,Typeable)

instance Show ParseError where
    show (ParseError line col err) = "Parse error: " ++ err ++ ":" ++ show line ++ ":" ++ show col

parsecParseError :: P.ParseError -> ParseError
parsecParseError x = ParseError (P.sourceLine pos) (P.sourceColumn pos) (show x)
    where pos = P.errorPos x
