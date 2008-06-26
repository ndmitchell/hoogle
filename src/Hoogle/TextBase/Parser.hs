
module Hoogle.TextBase.Parser(parseTextBase,parseTextBaseString) where

import General.Code
import Hoogle.TextBase.Type
import Hoogle.TypeSig.All
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


parseTextBase :: FilePath -> IO (Either ParseError TextBase)
parseTextBase file = do
    src <- readFile file
    return $ parseTextItems file src


parseTextBaseString :: String -> Either ParseError TextBase
parseTextBaseString = parseTextItems ""


parseTextItems :: FilePath -> String -> Either ParseError TextBase
parseTextItems file = join . f [] . zip [1..] . lines
    where
        f com [] = []
        f com ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] is
            | "--   " `isPrefixOf` s && com /= [] = f (drop 5 s:com) is
            | "-- " `isPrefixOf` s = f [] is
            | all isSpace s = f [] is
            | otherwise = (case parse parsecTextItem file s of
                               Left y -> Left $ setErrorPos (setSourceLine (errorPos y) i) y
                               Right y -> Right [(y, unlines $ reverse com)])
                          : f [] is

        join xs | null err = Right $ concat items
                | otherwise = Left $ head err
            where (err,items) = unzipEithers xs


isTextItem :: String -> Bool
isTextItem x = not $ null x2 || "--" `isPrefixOf` x2
    where x2 = dropWhile isSpace x


parsecTextItem :: Parser TextItem
parsecTextItem = attribute <|> item
    where
        attribute = do x <- try (char '@' >> satisfy isAlpha)
                       xs <- many (satisfy isAlpha)
                       spaces
                       ys <- many (noneOf "\n")
                       return $ ItemAttribute (x:xs) ys

        item = modu <|> clas <|> inst <|> newtyp <|> typ <|> dat <|> func
        
        begin x = try (string x >> many1 space)

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
                  spaces
                  string "::"
                  spaces
                  typ <- parsecTypeSig
                  return $ ItemFunc name typ
        
        keysymbol = many1 $ satisfy (\x -> isSymbol x || x `elem` ascSymbol)
        ascSymbol = "!#$%&*+./<=>?@\\^|-~:"

        keyword = do x <- letter
                     xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                     return (x:xs)
