{-|
    Parse a list of flags supported in any of the front ends.

    Returns the flags in a data structure, along with any invalid flags
-}

module CmdLine.Flag(
    CmdFlag(..),
    flagsWebArgs, flagsWebQuery, flagsCmdLine
    ) where

import Control.Monad
import Data.Char


data CmdFlag = Version         -- ^ Version information
             | Web             -- ^ Operate as a CGI process
             | Help            -- ^ Help text
             | Color Bool      -- ^ Colors on the console
             | Start Int       -- ^ First result to show
             | Count Int       -- ^ Number of results to show
               deriving (Eq {-! Enum !-} )


-- | flags that are passed in through web arguments,
--   i.e. ?foo=bar&...
flagsWebArgs :: [(String,String)] -> ([CmdFlag],[String])
flagsWebArgs xs = ([], map fst xs)


-- | flags that are given in the web query string
flagsWebQuery :: [(String,String)] -> ([CmdFlag],[String])
flagsWebQuery = flagsWebArgs


-- | flags that are given in a query on the command line
flagsCmdLine :: [(String,String)] -> ([CmdFlag],[String])
flagsCmdLine = parseFlags


-- check no flag is specified twice
parseFlags :: [(String,String)] -> ([CmdFlag],[String])
parseFlags xs = f [] xs
    where
        f seen [] = ([], [])
        f seen ((key,val):xs) = case parseFlag key val of
                Just x | xe `notElem` seen -> (x:a,b)
                    where xe = fromEnum x
                          (a,b) = f (xe:seen) xs
                Nothing -> (a,key:b)
                    where (a,b) = f seen xs


parseFlag :: String -> String -> Maybe CmdFlag
parseFlag key val
    | test ["v","ver","version"] = f0 Version
    | test ["?","h","help"] = f0 Help
    | test ["w","web"] = f0 Web
    | test ["c","col","color","colour"] = f1 parseBool Color
    | test ["s","start"] = f1 parseUint Start
    | test ["n","count","length","len"] = f1 parseUint Count
    | otherwise = Nothing
    where
        key2 = map toLower key
        test = elem key2
        
        f0 res | null val = Just res
               | otherwise = Nothing

        f1 f res = liftM res $ f val


parseUint :: String -> Maybe Int
parseUint x = case parseInt x of
                   Just y | y >= 0 -> Just y
                   _ -> Nothing


parseInt :: String -> Maybe Int
parseInt x = case reads x of
                  [(a,"")] -> Just a
                  _ -> Nothing


parseBool :: String -> Maybe Bool
parseBool v | v2 `elem` ["","yes","1","true","meep"] = Just True
            | v2 `elem` ["no","0","false","moop"] = Just False 
            | otherwise = Nothing
    where v2 = map toLower v


--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 83803175

instance Enum CmdFlag
    where toEnum 0 = Version{}
          toEnum 1 = Web{}
          toEnum 2 = Help{}
          toEnum 3 = Color{}
          toEnum 4 = Start{}
          toEnum 5 = Count{}
          toEnum n = error ((++) "toEnum " ((++) (show n) ", not defined for CmdFlag"))
          fromEnum (Version {}) = 0
          fromEnum (Web {}) = 1
          fromEnum (Help {}) = 2
          fromEnum (Color {}) = 3
          fromEnum (Start {}) = 4
          fromEnum (Count {}) = 5
