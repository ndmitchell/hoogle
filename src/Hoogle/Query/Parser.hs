
module Hoogle.Query.Parser(parseQuery, parseCmdLineQuery, parsecQuery) where

import General.Code hiding (merge,merges)
import Hoogle.Query.Type
import Hoogle.TypeSig.All
import Text.ParserCombinators.Parsec


ascSymbols = "!#$%&*+./<=>?@\\^|-~"

parseQuery :: String -> Either ParseError Query
parseQuery input = parse parsecQuery "" input


-- TODO: I don't think this handles spaces/quotes properly in the right
--       places.
parseCmdLineQuery :: [String] -> Either ParseError Query
parseCmdLineQuery args = parseQuery $ unwords $ map f args
    where
        f x | any isSpace x && ("--" `isPrefixOf` x || "/" `isPrefixOf` x) = "\"" ++ x ++ "\""
            | otherwise = x


blank = Query [] [] Nothing [] []

merge (Query a1 b1 c1 d1 e1) (Query a2 b2 c2 d2 e2) =
        Query (a1++a2) (b1++b2) (c1 `mplus` c2) (d1++d2) (e1++e2)

merges xs = foldr merge blank xs


parsecQuery :: Parser Query
parsecQuery = do spaces ; try (end names) <|> (end types)
    where
        end f = do x <- f; eof; return x
    
        names = do a <- many (flag <|> name)
                   b <- option blank (string "::" >> spaces >> types)
                   let res@Query{names=names} = merge (merges a) b
                       (op,nop) = partition ((`elem` ascSymbols) . head) names
                   if op /= [] && nop /= []
                       then fail "Combination of operators and names"
                       else return res
        
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
                  return blank{flags=[Flag (map toLower name) extra]}
            where
                flagExtra = quoteStr <|> spaceStr
                quoteStr = between (char '\"') (char '\"') (many anyChar)
                spaceStr = manyTill anyChar ((space >> return ()) <|> eof)

        scope = do pm <- oneOf "+-"
                   let aPackage = if pm == '+' then PlusPackage else MinusPackage
                       aModule  = if pm == '+' then PlusModule  else MinusModule
                   modu <- modname
                   case modu of
                       [x] -> return $ blank{scope=[if isLower (head x) then aPackage x else aModule [x]]}
                       xs -> return $ blank{scope=[aModule xs]}

        modname = keyword `sepBy1` (char '.')


-- TODO: Should share this definition with Hoogle.TypeSig.Parser
keyword = do x <- letter
             xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
             return (x:xs)

