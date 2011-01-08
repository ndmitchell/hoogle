{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.ParseError where

import Hoogle.Type.TagStr
import Data.Data

-- | Data type representing a parse error. All indecies are 1-based.
data ParseError = ParseError
    {lineNo :: Int -- ^ Line number on which the error occured, 1 for the first line of a file.
    ,columnNo :: Int -- ^ Column number on which the error occured, 1 for the first character of a line.
    ,errorMessage :: String -- ^ Error message caused by the parse error.
    ,parseInput :: TagStr -- ^ Input string which caused the error - sometimes with a 'TagEmph' to indicate which part was incorrect.
    } deriving (Ord,Eq,Data,Typeable)

instance Show ParseError where
    show (ParseError line col err _) = "Parse error " ++ show line ++ ":" ++ show col ++ ": " ++ err


parseErrorWith :: Int -> Int -> String -> String -> ParseError
parseErrorWith line col err text = ParseError line col err $ Tags [Str pre, TagEmph $ Str $ post ++ post2]
    where
        (pre,post) = splitAt (col-1) text
        post2 = if null post then "   " else ""
