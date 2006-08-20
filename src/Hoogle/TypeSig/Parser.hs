
module Hoogle.TypeSig.Parser(parsecTypeSig, parseTypeSig) where

import Hoogle.TypeSig.Type
import Text.ParserCombinators.Parsec
import Data.Char


parseTypeSig :: String -> Either ParseError TypeSig
parseTypeSig input = parse (do x <- parsecTypeSig ; eof ; return x) "" input


parsecTypeSig :: Parser TypeSig
parsecTypeSig = do whites
                   c <- context
                   t <- typ0
                   return $ normaliseTypeSig $ TypeSig c t
    where
        -- all the parser must swallow up all trailing white space after them
        context = try acontext <|> return []
        
        acontext = do x <- conitems <|> (conitem >>= return . (:[]))
                      white $ string "=>"
                      return x
        
        conitems = between (wchar '(') (wchar ')') $ conitem `sepBy1` (wchar ',')
        conitem = typ1
        
    
        typ0 = function
        typ1 = application
        typ2 = tuple <|> list <|> atom
    
        -- match (a,b) and (,)
        -- also pick up ( -> )
        tuple = do wchar '('
                   (do wchar ','
                       xs <- many $ wchar ','
                       wchar ')'
                       return $ tLit (length xs + 1)
                    ) <|>
                    (do white $ string "->"
                        wchar ')'
                        return $ TLit "->"
                    ) <|>
                    (do xs <- typ0 `sepBy` wchar ','
                        wchar ')'
                        return $ case xs of
                            [] -> TLit "()"
                            [x] -> x
                            xs -> TApp (tLit $ length xs - 1) xs
                    )
            where
                tLit n = TLit $ "(" ++ replicate n ',' ++ ")"
            
            
        atom = do x <- satisfy isAlpha
                  xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                  whites
                  return $ (if isLower x then TVar else TLit) (x:xs)

        -- may be [a], or [] (then application takes the a after it)
        list = do wchar '['
                  (char ']' >> return (TLit "[]")) <|> (do
                      x <- typ0
                      wchar ']'
                      return $ TApp (TLit "[]") [x])

        application = do (x:xs) <- many1 (white typ2)
                         return $ TApp x xs

        function = do xs <- typ1 `sepBy1` (try $ char '-' >> oneOf ">#" >> whites)
                      return $ case xs of
                         [x] -> x
                         xs -> TFun xs

        wchar c = white $ char c
        white x = do y <- x ; whites ; return y

        whites = many whiteChar
        whiteChar = oneOf " \v\f\t\r"
