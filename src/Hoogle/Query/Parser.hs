
module Hoogle.Query.Parser(parseQuery) where

import General.Code
import Hoogle.Query.Type
import Hoogle.Type.All as Hoogle
import Text.ParserCombinators.Parsec hiding (ParseError)
import qualified Text.ParserCombinators.Parsec as Parsec


parseQuery :: String -> Either ParseError Query
parseQuery x = case bracketer x of
    Left err -> Left err
    Right _ -> case parse parsecQuery "" x of
        Left err -> Left $ toParseError x err
        Right x -> Right x


toParseError :: String -> Parsec.ParseError -> Hoogle.ParseError
toParseError src x = parseErrorWith (sourceLine pos) (sourceColumn pos) (show x) src
    where pos = errorPos x


ascSymbols = "->!#$%&*+./<=?@\\^|~:"


optionBool :: Parser a -> Parser Bool
optionBool p = (p >> return True) <|> return False


joinQuery (Query a1 b1 c1) (Query a2 b2 c2) = Query (a1++a2) (b1 `mplus` b2) (c1++c2)
joinQueries = foldr joinQuery blankQuery


---------------------------------------------------------------------
-- QUERY PARSEC

parsecQuery :: Parser Query
parsecQuery = do spaces ; try (end names) <|> (end types)
    where
        end f = do x <- f; eof; return x
    
        names = do a <- many (flag <|> name)
                   b <- option blankQuery (string "::" >> spaces >> types)
                   let res@Query{names=names} = joinQuery (joinQueries a) b
                       (op,nop) = partition ((`elem` ascSymbols) . head) names
                   if op /= [] && nop /= []
                       then fail "Combination of operators and names"
                       else return res
        
        name = (do x <- operator ; spaces ; return blankQuery{names=[x]})
               <|>
               (do xs <- keyword `sepBy1` (char '.') ; spaces
                   return $ case xs of
                       [x] -> blankQuery{names=[x]}
                       xs -> blankQuery{names=[last xs],scope=[PlusModule (init xs)]}
               )
        
        operator = between (char '(') (char ')') op <|> op

        op = try $ do
            res <- many1 $ satisfy (`elem` ascSymbols)
            if res == "::" then fail ":: is not an operator name" else return res
        
        types = do a <- flags
                   b <- parsecTypeSig
                   c <- flags
                   return $ joinQueries [a,blankQuery{typeSig=Just b},c]

        flag = try $ do x <- parseFlagScope ; spaces ; return x
        flags = fmap joinQueries $ many flag


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
        [x] -> return $ blankQuery{scope=[if isLower (head x) then aPackage x else aModule [x]]}
        xs -> return $ blankQuery{scope=[aModule xs]}


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


--------------------------------------------------------------------
-- BRACKETER

openBrackets = "(["
shutBrackets = ")]"


data Bracket = Bracket Char [Bracket] -- Char is one of '(' or '['
             | NoBracket Char
               deriving Show

bracketer :: String -> Either ParseError [Bracket]
bracketer xs = case readBracket (1,xs) of
    Left (msg,from,to) -> f msg from to
    Right (res,(i,_:_)) -> f "Unexpected closing bracket" i (length xs)
    Right (res,_) -> Right res
    where
        f msg from to = Left $ ParseError 1 from msg $ formatTags xs [((from-1,to-1),TagEmph)]


type StrPos = (Int,String)

-- Given a list of pos/chars return either a failure (msg,start,end) or some bracket and the remaining chars
readBracket :: StrPos -> Either (String,Int,Int) ([Bracket], StrPos)
readBracket (i,"") = Right ([],(i,""))
readBracket (i, x:xs)
    | x `elem` shutBrackets = Right ([], (i,x:xs))
    | x `elem` openBrackets = case readBracket (i+1,xs) of
        Left e -> Left e
        Right (_, (j,[])) -> Left ("Closing bracket expected", i, j)
        Right (res, (j,y:ys))
            | elemIndex x openBrackets /= elemIndex y shutBrackets -> Left ("Bracket mismatch", i, j+1)
            | otherwise -> case readBracket (j+1,ys) of
                Left e -> Left e
                Right (a,b) -> Right (Bracket x res:a, b)
    | otherwise = case readBracket (i+1,xs) of
        Left e -> Left e
        Right (a,b) -> Right (NoBracket x:a, b)
