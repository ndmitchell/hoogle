{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

module Hoogle.Parser (
    parser,
    parseConType,
    validLine
    ) where

import Hoogle.Lexer
import Hoogle.TypeSig



-- | Is this line a content based line
validLine :: String -> Bool
validLine "" = False
validLine ('-':'-':_) = False
validLine _ = True


-- | Parse one line of Lexemes at a time and convert it to an item, or raise a parse error
parser :: String -> Either Item String
parser x = case lexer x of
               Left x -> parse x
               Right x -> Right x

parse :: [Lexeme] -> Either Item String
parse (VarName "module":TypeName x:[]) = Left $ Module (splitOn '.' x)

parse (VarName "class":x) = Left $ Class (readConType x)

parse (VarName "instance":x) = Left $ Instance (readConType x)

parse (VarName "keyword":xs) = Left $ Keyword $ concatMap show xs

parse (VarName "newtype":x) = Left $ Data True  (readConType x)
parse (VarName "data"   :x) = Left $ Data False (readConType x)

parse (VarName "type":x) = Left $ TypeAlias name (map (\(VarName a) -> a) args) (readConType b)
    where (TypeName name:args,_:b) = break (== EqSymbol) x

parse (TypeName x:TypeColon:typ) = Left $ Func x (readConType typ)
parse (VarName  x:TypeColon:typ) = Left $ Func x (readConType typ)

-- a data value produced by Haddock being foolish
parse [VarName  x] = Right ""
parse [TypeName x] = Right ""


parse [] = Right "Parse error: unexpected empty line"
parse (x:xs) = Right $ "Parse error: doesn't start with a keyword, or have a type annotation, " ++ show x


parseConType :: String -> Either ConType String
parseConType xs = case lexer xs of
                      Left x -> Left (readConType x)
                      Right x -> Right x


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn e xs = if null b then [a] else a : splitOn e (tail b)
    where (a,b) = break (== e) xs


readConType :: [Lexeme] -> (Constraint, Type)
readConType xs = (con, readType res)
    where (con, res) = readConstraint xs


readConstraint :: [Lexeme] -> (Constraint, [Lexeme])
readConstraint x = if not (EqArrow `elem` x) then ([],x)
                   else (f (readType a) ++ con, lexe)
    where
        (a,b) = break (== EqArrow) x
        (con,lexe) = readConstraint (tail b)
        
        f (TList (TLit ",":xs)) = xs
        f x = [x]



readType :: [Lexeme] -> Type
readType x = f $ bracket $ filter (/= ExSymbol) x
    where
        f :: [Bracket Lexeme] -> Type
        f xs = if not (singleton tups) then TList (TLit ",":map f tups)
               else if not (singleton func) then TList (TLit "->":map f func)
               else if singleton xs then g (head xs)
               else if null xs then TVar "_"
               else TList (map g xs)
            where
                tups = splitOn (BItem Comma    ) xs
                func = splitOn (BItem LineArrow) xs
        
        g (Bracket OpenRound  x) = f x
        g (Bracket OpenSquare []) = TLit "[]"
        g (Bracket OpenSquare x) = TList [TLit "[]", f x]
        g (BItem (TypeName x)) = TLit x
        g (BItem (VarName  x)) = TVar x
        g x = error $ show x
    

singleton [x] = True
singleton _ = False


data Bracket x = Bracket x [Bracket x]
               | BItem x
               deriving (Eq, Show)


isOpen x = x `elem` [OpenRound, OpenSquare]
isShut x = x `elem` [ShutRound, ShutSquare]

bracket :: [Lexeme] -> [Bracket Lexeme]
bracket xs = let (a,[]) = g xs in a
    where
        f :: [Lexeme] -> (Bracket Lexeme, [Lexeme])
        f (x:xs) | isOpen x = let (a,b) = g xs in (Bracket x a, b)
        f (x:xs) = (BItem x, xs)
        
        
        g :: [Lexeme] -> ([Bracket Lexeme], [Lexeme])
        g (x:xs) | isShut x = ([], xs)
        g [] = ([], [])
        g xs = let (a,b) = f xs ; (c,d) = g b in (a:c,d)
