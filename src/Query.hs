{-# LANGUAGE PatternGuards #-}

module Query(Query(..), Scope(..), parseQuery, parseScope) where

import Data.List
import Data.Tuple.Extra
import Language.Haskell.Exts
import Control.Monad
import Data.Monoid

---------------------------------------------------------------------
-- DATA TYPE

data Query = Query [Scope] [String] (Maybe Type) deriving (Show,Eq)

instance Monoid Query where
    mempty = Query [] [] Nothing
    mappend (Query x1 x2 x3) (Query y1 y2 y3) = Query (x1 ++ y1) (x2 ++ y2) (x3 `mplus` y3)

data Scope = Scope Bool String String deriving (Show,Eq)


---------------------------------------------------------------------
-- PARSER

parseQuery :: String -> Query
parseQuery x | "::":xs <- names = Query cat [] (Just $ fromParseResult $ parseType $ unwords xs)
             | otherwise = Query cat names Nothing
    where
        (cat,names) = first (map parseScope) $ partition (\x -> not (":" `isPrefixOf` x) && ':' `elem` x) $ words x


parseScope :: String -> Scope
parseScope ('+':xs) = parseScope xs
parseScope ('-':xs) = let Scope _ a b = parseScope xs in Scope False a b
parseScope xs = let (a,_:b) = break (== ':') xs in Scope True a b
