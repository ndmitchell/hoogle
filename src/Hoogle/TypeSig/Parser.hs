
module Hoogle.TypeSig.Parser(parsecTypeSig, parseTypeSig) where

import Hoogle.TypeSig.Type
import Text.ParserCombinators.Parsec
import General.Code


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
                      white $ char '=' >> oneOf "#>"
                      return x
        
        conitems = between (wchar '(') (wchar ')') $ conitem `sepBy1` (wchar ',')
        conitem = typ1
        
    
        typ0 = function
        typ1 = application
        typ2 = forAll <|> tuple <|> list <|> atom <|> bang

        bang = wchar '!' >> typ2
        
        forAll = do try (white $ string "forall")
                    many atom
                    wchar '.'
                    TypeSig con typ <- parsecTypeSig
                    return typ


        -- match (a,b) and (,)
        -- also pick up ( -> )
        tuple = do char '('
                   hash <- optionBool $ char '#'
                   let close = white $ string $ ['#'|hash] ++ ")"
                   whites 
                   (do wchar ','
                       xs <- many $ wchar ','
                       close
                       return $ tLit hash (length xs + 1)
                    ) <|>
                    (do sym <- white keysymbol
                        close
                        return $ TLit sym
                    ) <|>
                    (do xs <- typ0 `sepBy` wchar ','
                        close
                        return $ case xs of
                            [] -> TLit "()"
                            [x] -> x
                            xs -> TApp (tLit hash $ length xs - 1) xs
                    )
            where
                tLit hash n = TLit $ "(" ++ h ++ replicate n ',' ++ h ++ ")"
                    where h = ['#'|hash]
            
            
        atom = do x <- satisfy isAlpha
                  xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                  whites
                  return $ (if isLower x then TVar else TLit) (x:xs)

        -- may be [a], or [] (then application takes the a after it)
        list = do char '['
                  colon <- optionBool $ char ':'
                  spaces
                  let close = white $ string $ [':'|colon] ++ "]"
                      lit = TLit $ if colon then "[::]" else "[]"
                  (close >> return lit) <|> (do
                      x <- typ0
                      close
                      return $ TApp lit [x])

        application = do (x:xs) <- many1 (white typ2)
                         return $ TApp x xs

        function = do lhs <- typ1
                      (do op <- white keysymbol; rhs <- function; return $ TApp (TLit op) [lhs,rhs])
                          <|> return lhs

        wchar c = white $ char c
        white x = do y <- x ; whites ; return y

        whites = many whiteChar
        whiteChar = oneOf " \v\f\t\r"

        keysymbol = try $ do
            x <- many1 $ satisfy (\x -> isSymbol x || x `elem` ascSymbol)
            if x `elem` ["->","-#"] then return "->" -- fast shortcut for arrows
             else if x `elem` reservedSym then fail "Bad symbol"
             else return x
        ascSymbol = "->#!$%&*+./<=?@\\^|-~:"
        reservedSym = ["::","=>","=#",".","=","#",":","-","+","/","--"]


optionBool p = (p >> return True) <|> return False
