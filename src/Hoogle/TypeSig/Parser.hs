
module Hoogle.TypeSig.Parser(parsecTypeSig, parseTypeSig) where

import Hoogle.TypeSig.Type
import Text.ParserCombinators.Parsec
import Data.Char


parseTypeSig :: String -> Either ParseError TypeSig
parseTypeSig input = parse (do x <- parsecTypeSig ; eof ; return x) "" input


parsecTypeSig :: Parser TypeSig
parsecTypeSig = spaces >> typ0 >>= return . normaliseTypeSig . TypeSig []
    where
        -- all the parser must swallow up all trailing white space after them
    
        typ0 = function
        typ1 = application
        typ2 = tuple <|> list <|> atom
    
        tuple = do wchar '('
                   xs <- typ0 `sepBy` wchar ','
                   wchar ')'
                   return $ case xs of
                      [] -> TLit "()"
                      [x] -> x
                      xs -> TApp (TLit $ "(" ++ replicate (length xs - 1) ',' ++ ")") xs

        atom = do x <- satisfy isAlpha
                  xs <- many $ satisfy (\x -> isAlphaNum x || x == '_')
                  spaces
                  return $ (if isLower x then TVar else TLit) (x:xs)

        -- may be [a], or [] (then application takes the a after it)
        list = do wchar '['
                  (char ']' >> return (TLit "[]")) <|> (do
                      x <- typ0
                      wchar ']'
                      return $ TApp (TLit "[]") [x])

        application = do (x:xs) <- many1 (white typ2)
                         return $ TApp x xs

        function = do xs <- typ1 `sepBy1` (try $ char '-' >> oneOf ">#" >> spaces)
                      return $ case xs of
                         [x] -> x
                         xs -> TFun xs

        wchar c = white $ char c
        white x = do y <- x ; spaces ; return y

