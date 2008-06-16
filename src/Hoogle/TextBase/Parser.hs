
module Hoogle.TextBase.Parser(parseTextBase) where

import Hoogle.TextBase.Type
import Hoogle.TypeSig.All
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad


parseTextBase :: FilePath -> IO (Either ParseError TextBase)
parseTextBase = parseFromFile parsecTextBase


parsecTextBase :: Parser TextBase
parsecTextBase = do x <- anyLineSpace `sepBy` newline
                    many newline
                    eof
                    return $ concat x
    where
        whiteChar = oneOf " \v\f\t\r"
        whites1 = skipMany1 whiteChar
        whites = skipMany whiteChar
        
        anyLineSpace = do whites ; x <- anyLine ; whites ; return x
        anyLine = comment <|> attribute <|> liftM (:[]) item <|> blank
    
        comment = string "--" >> skipMany (noneOf "\n") >> return []
        blank = return []

        attribute = do x <- try (char '@' >> satisfy isAlpha)
                       xs <- many (satisfy isAlpha)
                       whites
                       ys <- many (noneOf "\n")
                       return [ItemAttribute xs ys]

        item = modu <|> clas <|> inst <|> newtyp <|> typ <|> dat <|> func
        
        begin x = try (string x >> whites1)

        modu = begin "module" >> liftM ItemModule (keyword `sepBy1` char '.')
        clas = begin "class" >> liftM ItemClass parsecTypeSig
        inst = begin "instance" >> liftM ItemInstance parsecTypeSig
        
        typ = do begin "type"
                 a <- parsecTypeSig
                 char '='
                 b <- parsecTypeSig
                 return $ ItemAlias a b
        
        newtyp = begin "newtype" >> dataAny NewTypeKeyword
        dat = begin "data" >> dataAny DataKeyword
        
        dataAny d = liftM (ItemData d) parsecTypeSig

        
        func = do name <- between (char '(') (char ')') keysymbol <|> keysymbol <|> keyword
                  whites
                  (do string "::" ; whites
                      typ <- parsecTypeSig
                      return $ ItemFunc name typ
                    ) <|> (return $ ItemKeyword name)
        
        keysymbol = many1 $ satisfy (\x -> isSymbol x || x `elem` ascSymbol)
        ascSymbol = "!#$%&*+./<=>?@\\^|-~:"

        keyword = do x <- letter
                     xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                     return (x:xs)
