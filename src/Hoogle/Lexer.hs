{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{- |
    A very basic lexer, splits things up into various 'Lexeme' pieces.
    Uses the underlying Haskell lex function.
-}

module Hoogle.Lexer (
    Lexeme(..),
    lexer
    ) where

import Prelude
import Char

-- | The data structure for a lexeme
data Lexeme = OpenSquare  -- ^ \[
            | ShutSquare  -- ^ \]
            | OpenRound   -- ^ \(
            | ShutRound   -- ^ \)
            | Comma       -- ^ \,
            | LineArrow   -- ^ \->
            | EqArrow     -- ^ \=>
            | EqSymbol    -- ^ \=
            | TypeColon   -- ^ \::
            | ExSymbol    -- ^ \!
            | TypeName String -- ^ Ctor
            | VarName String  -- ^ func
            deriving (Eq)


instance Show Lexeme where
    show OpenSquare  = "["
    show ShutSquare  = "]"
    show OpenRound   = "("
    show ShutRound   = ")"
    show Comma       = ","
    show LineArrow   = "->"
    show EqArrow     = "=>"
    show EqSymbol    = "="
    show TypeColon   = "::"
    show ExSymbol    = "!"
    show (TypeName x) = x
    show (VarName  x) = x


-- | The main lexer
lexer :: String -> Either [Lexeme] String
lexer ('(':xs) | all isSymbolChar a = 
        if null bs then Right "Parse Error: Missing closing bracket ')'"
        else case lexRest [] (tail bs) of
                  Left x -> Left $ VarName ('(':a++")") : x
                  Right x -> Right x
    where (a,bs) = break (== ')') xs
    
lexer x = lexRest [] x


isSymbolChar x = not (isSpace x) &&
                 not (x == ',') &&
                 not (isAlphaNum x) &&
                 not (x == '_')


lexRest :: [Lexeme] -> String -> Either [Lexeme] String
lexRest acc (' ':xs) = lexRest acc xs
lexRest acc ('(':')':xs) = lexRest (TypeName "()":acc) xs
lexRest acc ('-':'>':xs) = lexRest (LineArrow  :acc) xs
lexRest acc ('=':'>':xs) = lexRest (EqArrow    :acc) xs
lexRest acc (':':':':xs) = lexRest (TypeColon  :acc) xs
lexRest acc ('=':xs)     = lexRest (EqSymbol   :acc) xs
lexRest acc ('!':xs)     = lexRest (ExSymbol   :acc) xs
lexRest acc ('[':xs)     = lexRest (OpenSquare :acc) xs
lexRest acc (']':xs)     = lexRest (ShutSquare :acc) xs
lexRest acc ('(':xs)     = lexRest (OpenRound  :acc) xs
lexRest acc (')':xs)     = lexRest (ShutRound  :acc) xs
lexRest acc (',':xs)     = lexRest (Comma      :acc) xs
lexRest acc (x:xs) | x == '_'      || (x >= 'a' && x <= 'z') = lexRest (VarName  a:acc) b
                   | x `elem` "#?" || (x >= 'A' && x <= 'Z') = lexRest (TypeName a:acc) b
                   where (a, b) = lexWord [x] xs
lexRest acc (x:xs) = Right $ "Parse Error: Unexpected character '" ++ take 10 (x:xs) ++ "'"
lexRest acc [] = Left $ reverse acc


lexWord :: String -> String -> (String, String)
lexWord acc [] = (reverse acc, "")
lexWord acc (x:xs) | isDigit x || isAlpha x || x `elem` "_.'#?" = lexWord (x:acc) xs
                   | otherwise = (reverse acc, x:xs)
                   

