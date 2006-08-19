
module Hoogle.Query.Parser(parseQuery, parseCmdlineQuery, parsecQuery) where

import Hoogle.Query.Type
import Hoogle.TypeSig.All
import Text.ParserCombinators.Parsec
import Data.Char
import Data.List
import Data.Maybe


ascSymbols = "!#$%&*+./<=>?@\\^|-~"

parseQuery :: String -> Either ParseError Query
parseQuery input = parse (do x <- parsecQuery ; eof ; return x) "" input


parseCmdlineQuery :: [String] -> Either ParseError Query
parseCmdlineQuery args = parseQuery $ concat $ intersperse " " $ map f args
    where
        f x | any isSpace x = "\"" ++ x ++ "\""
            | otherwise = x


blank = Query [] [] Nothing [] []

merge (Query a1 b1 c1 d1 e1) (Query a2 b2 c2 d2 e2) =
        Query (a1++a2) (b1++b2) c3 (d1++d2) (e1++e2)
    where
        c3 = listToMaybe $ maybeToList c1 ++ maybeToList c2

merges xs = foldr merge blank xs


parsecQuery :: Parser Query
parsecQuery = do spaces ; try names <|> types
    where
        names = do a <- many (flag <|> name)
                   b <- option blank (string "::" >> spaces >> types)
                   return (merge (merges a) b)
        
        name = (do x <- operator ; spaces ; return blank{names=[x]})
               <|>
               (do xs <- keyword `sepBy1` (char '.') ; spaces
                   return $ case xs of
                       [x] -> blank{names=[x]}
                       xs -> blank{names=[last xs],scope=[PlusModule (init xs)]}
               )
        
        operator = between (char '(') (char ')') op <|> op

        op = many1 $ satisfy (`elem` ascSymbols)
        
        types = do a <- flags
                   b <- parsecTypeSig
                   c <- flags
                   return $ merges [a,blank{typeSig=Just b},c]
        
        flag = do x <- parseFlagScope ; spaces ; return x
        flags = many flag >>= return . merges
                   


-- deal with the parsing of:
--     --count=30
--     /module
--     +Data.Map
parseFlagScope :: Parser Query
parseFlagScope = do x <- try scope <|> flag
                    spaces
                    return x
    where
        -- -n 30, --count=30, --count 30, /n 30, /count=30, /count 30
        -- either a flag or an itemType
        flag = do string "--" <|> string "/"
                  xs <- many1 letter
                  flagWith xs

        flagWith s | not $ null itms = return $ blank{items=[head itms]}
                   | null flgs = fail $ "Unrecognised flag, " ++ s
                   | otherwise = do
                        y <- case ft of
                            FlagNull x -> return x
                            FlagInt x -> do preParam
                                            y <- many1 (satisfy isDigit)
                                            return $ x (read y)
                            FlagStr x -> do preParam
                                            y <- quoteStr <|> spaceStr
                                            return $ x y
                        return $ blank{flags=[y]}
            where
                preParam = string "=" <|> many1 space
                
                quoteStr = between (char '\"') (char '\"') (many anyChar)
                spaceStr = manyTill anyChar space
            
                s2 = map toLower s
                ft = flagType $ head flgs
                flgs = if length s2 == 1
                       then [fi | fi <- flagInfos, head s2 `elem` flagChar fi]
                       else [fi | fi <- flagInfos, s2 == flagStr fi]
                itms = [b | (a,b) <- itemTypes, s2 `elem` a]


        scope = do pm <- oneOf "+-"
                   let aPackage = if pm == '+' then PlusPackage else MinusPackage
                       aModule  = if pm == '+' then PlusModule  else MinusModule
                   modu <- modname
                   case modu of
                       [[x]] | pm == '-' -> flagWith [x]
                       [x] -> return $ blank{scope=[if isLower (head x) then aPackage x else aModule [x]]}
                       xs -> return $ blank{scope=[aModule xs]}

        modname = keyword `sepBy1` (char '.')


keyword = do x <- letter
             xs <- many $ satisfy (\x -> isAlphaNum x || x == '_')
             return (x:xs)

