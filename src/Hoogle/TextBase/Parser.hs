
module Hoogle.TextBase.Parser(parseTextBase) where

import Hoogle.TextBase.Type
import Hoogle.TypeSig.All
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad


parseTextBase :: FilePath -> IO (Either ParseError TextBase)
parseTextBase file = parseFromFile parsecTextBase file


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
        anyLine = comment <|> liftM (:[]) item <|> blank
    
        comment = string "--" >> skipMany (noneOf "\n") >> return []
        blank = return []

        item = modu <|> clas <|> inst <|> newtyp <|> typ <|> dat <|> func
        
        begin x = try (string x >> whites1)

        modu = begin "module" >> liftM Module (keyword `sepBy1` char '.')
        clas = begin "class" >> liftM Class parsecTypeSig
        inst = begin "instance" >> liftM Instance parsecTypeSig
        
        typ = do begin "type"
                 a <- parsecTypeSig
                 char '='
                 b <- parsecTypeSig
                 return $ TypeAlias a b
        
        newtyp = begin "newtype" >> dataAny NewTypeKeyword
        dat = begin "data" >> dataAny DataKeyword
        
        dataAny d = liftM (Data d) parsecTypeSig

        
        func = do name <- between (char '(') (char ')') (many1 $ noneOf ")") <|> keyword
                  whites
                  (do string "::" ; whites
                      typ <- parsecTypeSig
                      return $ Func name typ
                    ) <|> (return $ Keyword name)
        
        
        keyword = do x <- letter
                     xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                     return (x:xs)
        
        