{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

{-|
    General text utility functions
-}

module Hoogle.TextUtil where

import Prelude
import Data.Maybe
import Data.Char
import Data.List


trim :: String -> String
trim = trimLeft . trimRight
trimLeft = dropWhile isSpace
trimRight = reverse . trimLeft . reverse


isSubstrOf :: Eq a => [a] -> [a] -> Bool
isSubstrOf find list = any (isPrefixOf find) (tails list)


splitList :: Eq a => [a] -> [a] -> [[a]]
splitList find str = if isJust q then a : splitList find b else [str]
    where
        q = splitPair find str
        Just (a, b) = q


splitPair :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitPair find str = f str
    where
        f [] = Nothing
        f x  | isPrefixOf find x = Just ([], drop (length find) x)
             | otherwise = if isJust q then Just (head x:a, b) else Nothing
                where
                    q = f (tail x)
                    Just (a, b) = q


indexOf find str = length $ takeWhile (not . isPrefixOf (lcase find)) (tails (lcase str))


lcase = map toLower
ucase = map toUpper


replace find with [] = []
replace find with str | find `isPrefixOf` str = with ++ replace find with (drop (length find) str)
                      | otherwise = head str : replace find with (tail str)




-- 0 based return
findNext :: Eq a => [[a]] -> [a] -> Maybe Int
findNext finds str = if null maxs then Nothing else Just (fst (head (sortBy compSnd maxs)))
    where
        maxs = mapMaybe f (zip [0..] finds)
        
        f (id, find) = if isJust q then Just (id, length (fst (fromJust q))) else Nothing
            where q = splitPair find str
        
        compSnd (_, a) (_, b) = compare a b


-- bracketing...

data Bracket = Bracket (Char, Char) [Bracket]
             | UnBracket String
             deriving (Show)

data PartBracket = PBracket [PartBracket]
                 | PUnBracket Char
                 deriving (Show)


bracketWith :: Char -> Char -> Bracket -> Bracket
bracketWith strt stop (Bracket x y) = Bracket x (map (bracketWith strt stop) y)
bracketWith strt stop (UnBracket x) = bracketString strt stop x


bracketString :: Char -> Char -> String -> Bracket
bracketString strt stop y = Bracket (strt, stop) (g "" res)
    where
        (a, b) = bracketPartial strt stop y
        res = a ++ (map PUnBracket b)
        
        g c (PBracket x:xs  ) = deal c ++ Bracket (strt, stop) (g "" x) : g "" xs
        g c (PUnBracket x:xs) = g (x:c) xs
        g c []                = deal c
        
        deal [] = []
        deal x  = [UnBracket (reverse x)]
        


bracketPartial :: Char -> Char -> String -> ([PartBracket], String)
bracketPartial strt stop y = f y
    where
        
        f [] = ([], "")
        
        f (x:xs) | x == strt = (PBracket a : c, d)
                                 where
                                    (a, b) = bracketPartial strt stop xs
                                    (c, d) = f b

        f (x:xs) | x == stop = ([], xs)
        
        f (x:xs) | otherwise = (PUnBracket x : a, b)
                                  where (a, b) = f xs
