{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input(parseInput) where

import Language.Haskell.Exts.Annotated
import Data.Char
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import Type


type HackageURL = String

parseInput :: HackageURL -> FilePath -> IO [Section (URL, Documentation, Item)]
parseInput hackage file = f [] . lines <$> readFile' file
    where
        f :: [String] -> [String] -> [Section (URL, Documentation, Item)]
        f com ((stripPrefix "-- " -> Just x):xs) = f (com ++ [x]) xs
        f com (x:xs) | all isSpace x = f [] xs
        f com (('@': (word1 -> (key,val))):xs) = Section key val : f [] xs
        f com ((stripPrefix "module " -> Just x):xs) = Section "module" x : f [] xs
        f com (x:xs) | ParseOk res <- parseDecl x = Item ("http:", unlines com, IDecl $ fmap (const ()) res) : f [] xs
        f com (x:xs) = f [] xs -- error $ "Could not parse line: " ++ show x
        f com [] = []
