
module Hoogle.Query.Parser2(parseQuery) where

import General.Base
import Hoogle.Query.Type2
import Hoogle.Type.All as Hoogle


parseQuery :: String -> ([Query], [Query] -> TagStr)
parseQuery = grouper . lexer

openBrackets = ["(#","[:","(","["]
shutBrackets = ["#)",":]",")","]"]


---------------------------------------------------------------------
-- LEXER

data Lexeme
    = Symbol String String
    | Ident String String
    | Scoped Bool Scope String
    | Bracket Bool String


-- Accepts:
-- bar ++ (++)
-- name.bar
-- name.++ name.(++) (name.++)
-- +foo -foo
-- +scope:foo -scope:foo scope:foo
lexer :: String -> [Lexeme]
lexer = runLexerEnd $ fmap concat $ many $ choice
    [do pm <- oneOf "+-"; scope <- modPkg
        do char ':'; name <- modPkg; return [Scoped (pm=='+') scp name | Just scp <- [readScope scope]] <|>
            return [Scoped (pm=='+') (if '.' `elem` name || all isUpper (take 1 name) then Module else Package) name]
    ]


modPkg = fmap (intercalate ".") $ ident `sepBy1` char '.'

ident = do
    x <- satisfy isAlpha
    xs <- many $ satisfy $ \x -> isAlphaNum x || x `elem` "_'#-"
    return $ x:xs


readScope :: String -> Maybe Scope
readScope _ = Nothing


---------------------------------------------------------------------
-- LEXICAL MONAD

sepBy1 a b = liftM2 (:) a (many $ b >> a)

oneOf xs = satisfy (`elem` xs)
char x = satisfy (== x)
string = mapM char

choice :: [Lexer a] -> Lexer a
choice = foldr1 (<|>)

runLexerEnd :: Lexer a -> String -> a
runLexerEnd f s = case runLexer f s of
    Just (v,"") -> v
    _ -> error $ "Lexing failed on: " ++ show s

newtype Lexer a = Lexer {runLexer :: String -> Maybe (a, String)}

instance Functor Lexer where
    fmap f (Lexer x) = Lexer $ fmap (first f) . x

instance Monad Lexer where
    a >>= f = Lexer $ \s -> case runLexer a s of
        Nothing -> Nothing
        Just (v,s) -> runLexer (f v) s
    return x = Lexer $ \s -> Just (x, s)
    fail _ = Lexer $ const Nothing

many a = Lexer $ Just . f
    where f s = case runLexer a s of
                    Nothing -> ([], s)
                    Just (x,s) -> first (x:) $ f s

(<|>) a b = Lexer $ \s -> runLexer a s `mplus` runLexer b s

fails :: Lexer a
fails = fail ""

eof :: Lexer Bool
eof = Lexer $ \s -> Just (null s, s)

satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = Lexer $ \s -> case s of
    x:xs | f x -> Just (x,xs)
    _ -> Nothing


---------------------------------------------------------------------
-- GROUPER

grouper :: [Lexeme] -> ([Query], [Query] -> TagStr)
grouper = undefined
--if there is a ::, split and continue that way
--otherwise if there are both symbol and ident names, try it as a type signature


---------------------------------------------------------------------
-- TYPE SIG

typeSig :: [Lexeme] -> (TypeSig, TypeSig -> TagStr)
typeSig _ = undefined
