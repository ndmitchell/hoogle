
module Hoogle.Query.Parser(parseQuery) where

import Data.Monoid
import General.Code
import Hoogle.Query.Type
import Hoogle.TypeSig.All
import qualified Hoogle.Util as U
import Text.ParserCombinators.Parsec


parseQuery :: String -> Either U.ParseError Query
parseQuery = either (Left . U.parsecParseError) Right . parse parsecQuery ""

ascSymbols = "->!#$%&*+./<=?@\\^|~:"

optionBool p = (p >> return True) <|> return False


---------------------------------------------------------------------
-- QUERY PARSEC

parsecQuery :: Parser Query
parsecQuery = do spaces ; try (end names) <|> (end types)
    where
        end f = do x <- f; eof; return x
    
        names = do a <- many (flag <|> name)
                   b <- option mempty (string "::" >> spaces >> types)
                   let res@Query{names=names} = mappend (mconcat a) b
                       (op,nop) = partition ((`elem` ascSymbols) . head) names
                   if op /= [] && nop /= []
                       then fail "Combination of operators and names"
                       else return res
        
        name = (do x <- operator ; spaces ; return mempty{names=[x]})
               <|>
               (do xs <- keyword `sepBy1` (char '.') ; spaces
                   return $ case xs of
                       [x] -> mempty{names=[x]}
                       xs -> mempty{names=[last xs],scope=[PlusModule (init xs)]}
               )
        
        operator = between (char '(') (char ')') op <|> op

        op = try $ do
            res <- many1 $ satisfy (`elem` ascSymbols)
            if res == "::" then fail ":: is not an operator name" else return res
        
        types = do a <- flags
                   b <- parsecTypeSig
                   c <- flags
                   return $ mconcat [a,mempty{typeSig=Just b},c]

        flag = try $ do x <- parseFlagScope ; spaces ; return x
        flags = many flag >>= return . mconcat


-- deal with the parsing of:
--     -package
--     +Module.Name
parseFlagScope :: Parser Query
parseFlagScope = do
    pm <- oneOf "+-"
    let aPackage = if pm == '+' then PlusPackage else MinusPackage
        aModule  = if pm == '+' then PlusModule  else MinusModule
        modname  = keyword `sepBy1` (char '.')
    modu <- modname
    case modu of
        [x] -> return $ mempty{scope=[if isLower (head x) then aPackage x else aModule [x]]}
        xs -> return $ mempty{scope=[aModule xs]}


keyword = do
    x <- letter
    xs <- many $ satisfy $ \x -> isAlphaNum x || x `elem` "_'#-"
    return $ x:xs


---------------------------------------------------------------------
-- TYPESIG PARSEC

parsecTypeSig :: Parser TypeSig
parsecTypeSig = do whites
                   c <- context
                   t <- typ0
                   return $ normaliseTypeSig $ TypeSig c t
    where
        -- all the parser must swallow up all trailing white space after them
        context = try acontext <|> return []
        
        acontext = do x <- conitems <|> fmap (:[]) conitem
                      white $ string "=>"
                      return x
        
        conitems = between (wchar '(') (wchar ')') $ conitem `sepBy1` wchar ','
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
            
            
        atom = do x <- satisfy (\x -> isAlpha x || x == '_')
                  xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                  whites
                  return $ (if isLower x || x == '_' then TVar else TLit) (x:xs)

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
            x <- many1 $ satisfy (\x -> isSymbol x || x `elem` ascSymbols)
            if x `elem` reservedSym then fail "Bad symbol" else return x
        reservedSym = ["::","=>",".","=","#",":","-","+","/","--"]
