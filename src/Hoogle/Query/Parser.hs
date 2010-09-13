
module Hoogle.Query.Parser(parseQuery, parseCmdLineQuery, parsecQuery) where

import General.Code hiding (merge,merges)
import Hoogle.Query.Type
import Hoogle.TypeSig.All
import Text.ParserCombinators.Parsec


ascSymbols = "!#$%&*+./<=>?@\\^|-~:"

parseQuery :: String -> Either ParseError Query
parseQuery = parse parsecQuery ""


-- TODO: I don't think this handles spaces/quotes properly in the right
--       places.
parseCmdLineQuery :: [String] -> Either ParseError Query
parseCmdLineQuery args = parseQuery $ unwords $ map f args
    where
        f x | any isSpace x && ("--" `isPrefixOf` x || "/" `isPrefixOf` x) = "\"" ++ x ++ "\""
            | otherwise = x


merge (Query a1 b1 c1 d1) (Query a2 b2 c2 d2) =
        Query (a1++a2) (b1++b2) (c1 `mplus` c2) (d1++d2)

merges = foldr merge blankQuery


parsecQuery :: Parser Query
parsecQuery = do spaces ; try (end names) <|> (end types)
    where
        end f = do x <- f; eof; return x
    
        names = do a <- many (flag <|> name)
                   b <- option blankQuery (string "::" >> spaces >> types)
                   let res@Query{names=names} = merge (merges a) b
                       (op,nop) = partition ((`elem` ascSymbols) . head) names
                   if op /= [] && nop /= []
                       then fail "Combination of operators and names"
                       else return res
        
        name = (do x <- operator ; spaces ; return blankQuery{names=[x]})
               <|>
               (do xs <- keyword False `sepBy1` (char '.') ; spaces
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
                   return $ merges [a,blankQuery{typeSig=Just b},c]

        flag = do x <- parseFlagScope ; spaces ; return x
        flags = many flag >>= return . merges
                   


-- deal with the parsing of:
--     --count=30
--     /module
--     /?  (special case)
--     +Data.Map
parseFlagScope :: Parser Query
parseFlagScope = do x <- try scope <|> try flag
                    spaces
                    return x
    where
        -- --n=30, --count=30, /flag, --flag
        -- either a flag or an itemType
        flag = do string "--" <|> string "/"
                  name <- many1 letter <|> string "?"
                  extra <- (do char '='; flagExtra) <|> (return "")
                  return blankQuery{flags=[Flag (map toLower name) extra]}
            where
                flagExtra = quoteStr <|> spaceStr
                quoteStr = between (char '\"') (char '\"') (many anyChar)
                spaceStr = manyTill anyChar ((space >> return ()) <|> eof)

        scope = do pm <- oneOf "+-"
                   let aPackage = if pm == '+' then PlusPackage else MinusPackage
                       aModule  = if pm == '+' then PlusModule  else MinusModule
                   modu <- modname
                   case modu of
                       [x] -> return $ blankQuery{scope=[if isLower (head x) then aPackage x else aModule [x]]}
                       xs -> return $ blankQuery{scope=[aModule xs]}

        modname = keyword True `sepBy1` (char '.')


keyword hyphen = do
    x <- letter
    xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#" || (hyphen && x == '-'))
    return (x:xs)

