
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
        anyLine = comment <|> attribute <|> liftM (:[]) item <|> blank
    
        comment = string "--" >> skipMany (noneOf "\n") >> return []
        blank = return []

        attribute = do char '@'
                       x <- many1 (noneOf " ")
                       whites
                       xs <- many (noneOf "\n")
                       return [mkAttribute x xs]

        item = modu <|> clas <|> inst <|> newtyp <|> typ <|> dat <|> func
        
        begin x = try (string x >> whites1)

        modu = begin "module" >> liftM mkModule (keyword `sepBy1` char '.')
        clas = begin "class" >> liftM mkClass parsecTypeSig
        inst = begin "instance" >> liftM mkInstance parsecTypeSig
        
        typ = do begin "type"
                 a <- parsecTypeSig
                 char '='
                 b <- parsecTypeSig
                 return $ mkTypeAlias a b
        
        newtyp = begin "newtype" >> dataAny NewTypeKeyword
        dat = begin "data" >> dataAny DataKeyword
        
        dataAny d = liftM (mkData d) parsecTypeSig

        
        func = do name <- between (char '(') (char ')') (many1 $ noneOf ")") <|> keyword
                  whites
                  (do string "::" ; whites
                      typ <- parsecTypeSig
                      return $ mkFunc name typ
                    ) <|> (return $ mkKeyword name)
        
        
        keyword = do x <- letter
                     xs <- many $ satisfy (\x -> isAlphaNum x || x `elem` "_'#")
                     return (x:xs)
        

-- map from the parsed representation
-- to an item

mkModule :: [String] -> Item ()
mkModule xs = Item (Just (Module (init xs) 0)) (Just $ last xs) Nothing Nothing () ItemModule

mkClass :: TypeSig -> Item ()
mkClass x = Item Nothing (Just b) Nothing Nothing () (ItemClass a)
    where (a,b) = splitSig x

mkInstance :: TypeSig -> Item ()
mkInstance x = Item Nothing Nothing Nothing Nothing () (ItemInstance x)

mkTypeAlias :: TypeSig -> TypeSig -> Item ()
mkTypeAlias x y = Item Nothing (Just b) Nothing Nothing () (ItemAlias a (TypeAST y))
    where (a,b) = splitSig x

mkData :: DataKeyword -> TypeSig -> Item ()
mkData k x = Item Nothing (Just b) Nothing Nothing () (ItemData k a)
    where (a,b) = splitSig x

mkFunc :: String -> TypeSig -> Item ()
mkFunc x y = Item Nothing (Just x) (Just $ TypeAST y) Nothing () ItemFunc

mkKeyword :: String -> Item ()
mkKeyword x = Item Nothing (Just x) Nothing Nothing () ItemKeyword

mkAttribute :: String -> String -> Item ()
mkAttribute x y = Item Nothing Nothing Nothing Nothing () (ItemAttribute x y)


splitSig :: TypeSig -> (LHS, String)
splitSig (TypeSig con (TLit x)) = (LHS con [], x)
splitSig (TypeSig con (TApp (TLit x) xs)) = (LHS con [x | TVar x <- xs], x)

-- error case, think about proper handling
splitSig (TypeSig con x) = (LHS con [], show x)
