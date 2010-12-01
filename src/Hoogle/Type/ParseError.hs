{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.ParseError where

import Hoogle.Type.TagStr
import Data.Data

-- | 1 based
data ParseError = ParseError
    {lineNo :: Int
    ,columnNo :: Int
    ,errorMessage :: String
    ,parseInput :: TagStr
    } deriving (Ord,Eq,Data,Typeable)

instance Show ParseError where
    show (ParseError line col err _) = "Parse error " ++ show line ++ ":" ++ show col ++ ": " ++ err


emptyParseError :: ParseError
emptyParseError = ParseError 0 0 "" $ Str ""


parseErrorWith :: Int -> Int -> String -> String -> ParseError
parseErrorWith line col err text = ParseError line col err $ Tags [Str pre, TagEmph $ Str $ post ++ post2]
    where
        (pre,post) = splitAt (col-1) text
        post2 = if null post then "   " else ""
