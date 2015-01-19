{-# LANGUAGE PatternGuards, ViewPatterns, RecordWildCards #-}

module Query(Query(..), Scope(..), parseQuery, renderQuery, parseScope) where

import Data.List
import Language.Haskell.Exts
import Control.Monad
import Data.Monoid
import Data.Char
import Data.List.Extra
import Data.Generics.Uniplate.Data
import General.Util

---------------------------------------------------------------------
-- DATA TYPE

data Query = Query {queryName :: [String], queryType :: Maybe Type, queryScope :: [Scope]} deriving (Show,Eq)

instance Monoid Query where
    mempty = Query [] Nothing []
    mappend (Query x1 x2 x3) (Query y1 y2 y3) = Query (x1 ++ y1) (x2 `mplus` y2) (x3 ++ y3)

data Scope = Scope {scopeInclude :: Bool, scopeCategory :: String, scopeValue :: String} deriving (Show,Eq)


renderQuery :: Query -> String
renderQuery Query{..} = if null xs then "<i>No query</i>" else unwords xs
    where
        xs = queryName ++
             concat [["::",pretty t] | Just t <- [queryType]] ++
             [['-' | not scopeInclude] ++ scopeCategory ++ ":" ++ scopeValue | Scope{..} <- queryScope]


---------------------------------------------------------------------
-- PARSER

parseScope :: String -> Scope
parseScope xs = let (a,_:b) = break (== ':') xs in Scope True a b


parseQuery :: String -> Query
parseQuery x = Query nam typ scp
    where
        (scp,rest) = scope_ $ lexer x
        (nam,typ) = divide rest


openBrackets = ["(#","[:","(","["]
shutBrackets = ["#)",":]",")","]"]

isBracket x = x `elem` (openBrackets ++ shutBrackets)
isBracketPair x = x `elem` zipWith (++) openBrackets shutBrackets

isAlphas (x:xs) = isAlpha x
isAlphas [] = False

isSym x = x `elem` "->!#$%&*+./<=?@\\^|~:"

isSyms xs | isBracket xs || isBracketPair xs = False
isSyms (x:xs) = isSym x
isSyms [] = False

-- | Split into small lexical chunks.
--
-- > "Data.Map.(!)" ==> ["Data",".","Map",".","(","!",")"]
lexer :: String -> [String]
lexer ('(':',':xs) | (a,')':b) <- span (== ',') xs = ("(," ++ a ++ ")") : lexer b
lexer x | Just s <- fmap (bs !!) $ findIndex (`isPrefixOf` x) bs = s : lexer (drop (length s) x)
    where bs = zipWith (++) openBrackets shutBrackets ++ openBrackets ++ shutBrackets
lexer (x:xs)
    | isSpace x = " " : lexer (dropWhile isSpace xs)
    | isAlpha x || x == '_' = let (a,b) = span (\x -> isAlphaNum x || x `elem` "_'#-") xs in (x:a) : lexer b
    | isSym x = let (a,b) = span isSym xs in (x:a) : lexer b
    | x == ',' = "," : lexer xs
    | otherwise = lexer xs -- drop invalid bits
lexer [] = []


-- | Find and extract the scope annotations.
--
-- > +package
-- > +module
-- > name.bar
-- > name.++ name.(++) (name.++)
-- > +foo -foo
-- > +scope:foo -scope:foo scope:foo
scope_ :: [String] -> ([Scope], [String])
scope_ xs = case xs of
    (readPM -> Just pm):(readCat -> Just cat):":":(readMod -> Just (mod,rest)) -> add pm cat mod rest
    (readPM -> Just pm):(readMod -> Just (mod,rest)) -> add_ pm mod rest
    (readCat -> Just cat):":":(readMod -> Just (mod,rest)) -> add True cat mod rest
    "(":(readDots -> Just (scp,x:")":rest)) -> out ["(",x,")"] $ add_ True scp rest
    (readDots -> Just (scp,rest)) -> add_ True scp rest
    x:xs -> out [x] $ scope_ xs
    [] -> ([], [])
    where
        out xs (a,b) = (a,xs++b)
        add a b c rest = let (x,y) = scope_ rest in (Scope a b c : x, y)
        add_ a c rest = add a b c rest
            where b = if '.' `elem` c || any isUpper (take 1 c) then "module" else "package"

        readPM x = case x of "+" -> Just True; "-" -> Just False; _ -> Nothing

        readCat x | isAlphas x = Just $ if map toLower x `isPrefixOf` "module" then "module" else "package"
                  | otherwise = Nothing

        readMod (x:xs) | isAlphas x = Just $ case xs of
            ".":ys | Just (a,b) <- readMod ys -> (x ++ "." ++ a, b)
            _ -> (x,xs)
        readMod _ = Nothing

        readDots (x:xs) | isAlphas x = case xs of
            ".":ys | Just (a,b) <- readDots ys -> Just (x ++ "." ++ a, b)
            ('.':y):ys -> Just (x, [y | y /= ""] ++ ys)
            _ -> Nothing
        readDots _ = Nothing


-- | If everything is a name, or everything is a symbol, then you only have names.
divide :: [String] -> ([String], Maybe Type)
divide xs | all isAlphas ns = (ns, Nothing)
          | all isSyms ns = (ns, Nothing)
          | length ns == 1 = (ns, Nothing)
          | otherwise = case break (== "::") xs of
                (nam, _:rest) -> (names_ nam, typeSig_ rest)
                _ -> ([], typeSig_ xs)
    where ns = names_ xs


-- | Ignore brackets around symbols, and try to deal with tuple names.
names_ :: [String] -> [String]
names_ ("(":x:")":xs) = x : names_ xs
names_ (x:xs) = [x | x /= " "] ++ names_ xs
names_ [] = []

typeSig_ :: [String] -> Maybe Type
typeSig_ xs = case parseTypeWithMode parseMode $ unwords $ fixup $ filter (not . all isSpace) xs of
    ParseOk x -> Just $ transformBi (\v -> if v == Ident "__" then Ident "_" else v) x
    _ -> Nothing
    where
        fixup = underscore . closeBracket . completeFunc . completeArrow

        completeArrow (unsnoc -> Just (a,b)) | b `elem` ["-","="] = snoc a (b ++ ">")
        completeArrow x = x

        completeFunc (unsnoc -> Just (a,b)) | b `elem` ["->","=>"] = a ++ [b,"_"]
        completeFunc x = x

        closeBracket xs = xs ++ foldl f [] xs
            where f stack x | Just c <- lookup x (zip openBrackets shutBrackets) = c:stack
                  f (s:tack) x | x == s = tack
                  f stack x = stack

        underscore = replace ["_"] ["__"]

