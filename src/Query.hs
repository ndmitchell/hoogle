{-# LANGUAGE PatternGuards #-}

module Query(Query(..), Restrict(..), parseQuery, parseRestrict) where

import Data.List
import Data.Tuple.Extra
import Language.Haskell.Exts
import Control.Monad
import Data.Monoid


data Query = Query [Restrict] [String] (Maybe Type) deriving Show

instance Monoid Query where
    mempty = Query [] [] Nothing
    mappend (Query x1 x2 x3) (Query y1 y2 y3) = Query (x1 ++ y1) (x2 ++ y2) (x3 `mplus` y3)

data Restrict = Restrict Bool String String deriving Show

parseQuery :: String -> Query
parseQuery x | "::":xs <- names = Query cat [] (Just $ fromParseResult $ parseType $ unwords xs)
             | otherwise = Query cat names Nothing
    where
        (cat,names) = first (map parseRestrict) $ partition (\x -> not (":" `isPrefixOf` x) && ':' `elem` x) $ words x


parseRestrict :: String -> Restrict
parseRestrict ('+':xs) = parseRestrict xs
parseRestrict ('-':xs) = let Restrict _ a b = parseRestrict xs in Restrict False a b
parseRestrict xs = let (a,_:b) = break (== ':') xs in Restrict True a b
