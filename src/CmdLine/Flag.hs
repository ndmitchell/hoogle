{-|
    Parse a list of flags supported in any of the front ends.

    Returns the flags in a data structure, along with any invalid flags
-}

module CmdLine.Flag(
    failMessage,
    CmdFlag(..), flagsHelp,
    flagsWebArgs, flagsWebQuery, flagsCmdLine
    ) where

import Control.Monad
import Data.Char
import Data.Maybe
import General.Code
import System.Exit


-- useful command line auxiliary
failMessage :: [String] -> IO ()
failMessage msg = putStr (unlines msg) >> exitFailure

---------------------------------------------------------------------
-- The flags

data CmdFlag = Version           -- ^ Version information
             | Web               -- ^ Operate as a CGI process
             | Help              -- ^ Help text
             | Test              -- ^ Run the regression tests
             | Color Bool        -- ^ Colors on the console
             | Start Int         -- ^ First result to show
             | Count Int         -- ^ Number of results to show
             | Convert FilePath  -- ^ Convert a database
             | Output FilePath   -- ^ Output file
             | Dump FilePath     -- ^ Dump a database to a file
             | DataPath FilePath -- ^ Database location
             | Verbose           -- ^ Display verbose information
             | Info              -- ^ Display as much information as you can
               deriving (Eq {-! Enum !-} )


-- | In which circumstances are you allowed to pass this command
data Permission = PWebArgs | PWebQuery | PCmdLine
                  deriving Eq

data Argument = ArgNone CmdFlag
              | ArgBool (Bool     -> CmdFlag)
              | ArgInt  (Int      -> CmdFlag)
              | ArgNat  (Int      -> CmdFlag)
              | ArgPos  (Int      -> CmdFlag)
              | ArgFile (FilePath -> CmdFlag)

data FlagInfo = FlagInfo {
    argument :: Argument,
    names :: [String],
    permissions :: [Permission],
    description :: String
    }

flagInfo =
    [f (ArgNone Version) ["v","version","ver"] [PCmdLine] "Print out version information"
    ,f (ArgNone Help) ["?","help","h"] [PCmdLine] "Show help message"
    ,f (ArgNone Web) ["w","web"] [PCmdLine] "Run as though it was a CGI script"
    ,f (ArgBool Color) ["c","color","col","colour"] [PCmdLine] "Show color output (default=false)"
    ,f (ArgPos  Start) ["s","start"] [PCmdLine,PWebArgs] "First result to show (default=1)"
    ,f (ArgPos  Count) ["n","count","length","len"] [PCmdLine,PWebArgs] "Number of results to show (default=all)"
    ,f (ArgNone Test) ["test"] [PCmdLine] "Run the regression tests"
    ,f (ArgFile Convert) ["convert"] [PCmdLine] "Convert a database"
    ,f (ArgFile Output) ["output"] [PCmdLine] "Output file for convert"
    ,f (ArgFile Dump) ["dump"] [PCmdLine] "Dump a database for debugging"
    ,f (ArgFile DataPath) ["d","data"] [PCmdLine] "Database location"
    ,f (ArgNone Verbose) ["verbose"] [PCmdLine] "Display verbose information"
    ,f (ArgNone Info) ["i","info"] [PCmdLine] "Display full information on an entry"
    ]
    where f = FlagInfo


---------------------------------------------------------------------
-- Operations on Flags

-- | flags that are passed in through web arguments,
--   i.e. ?foo=bar&...
flagsWebArgs :: [(String,String)] -> ([CmdFlag],[String])
flagsWebArgs = parseFlags PWebArgs


-- | flags that are given in the web query string
flagsWebQuery :: [(String,String)] -> ([CmdFlag],[String])
flagsWebQuery = parseFlags PWebQuery


-- | flags that are given in a query on the command line
flagsCmdLine :: [(String,String)] -> ([CmdFlag],[String])
flagsCmdLine = parseFlags PCmdLine


flagsHelp :: String
flagsHelp = unlines $ map f res
    where
        f (a,b,c) = "  " ++ (if null a then "    " else "--" ++ a ++ ",") ++
                    " --" ++ b ++ replicate (maxLong - length b) ' ' ++
                    "  " ++ c

        maxLong = maximum $ map (length . snd3) res
        res = [ (shortOpt (names i), longOpt (names i) ++ typ (argument i), description i)
              | i <- flagInfo, PCmdLine `elem` permissions i]

        shortOpt ([x]:_) = [x]
        shortOpt _ = ""
        
        longOpt ([_]:x:_) = x
        longOpt (x:_) = x

        typ (ArgNone _) = ""
        typ (ArgInt  _) = "=INT"
        typ (ArgNat  _) = "=NAT"
        typ (ArgPos  _) = "=POS"
        typ (ArgBool _) = "=BOOL"
        typ (ArgFile _) = "=FILE"


---------------------------------------------------------------------
-- Parsing Flags

-- check no flag is specified twice
parseFlags :: Permission -> [(String,String)] -> ([CmdFlag],[String])
parseFlags perm xs = f [] xs
    where
        f seen [] = ([], [])
        f seen ((key,val):xs) = case parseFlag perm key val of
                Just x | xe `notElem` seen -> (x:a,b)
                    where xe = fromEnum x
                          (a,b) = f (xe:seen) xs
                Nothing -> (a,key:b)
                    where (a,b) = f seen xs


parseFlag :: Permission -> String -> String -> Maybe CmdFlag
parseFlag perm key val = do
    let key2 = map toLower key
    i <- listToMaybe [i | i <- flagInfo, key2 `elem` names i, perm `elem` permissions i]
    parseArg (argument i) val


parseArg :: Argument -> String -> Maybe CmdFlag
parseArg (ArgNone v) xs = if null xs then Just v else Nothing
parseArg (ArgBool v) xs = liftM v $ parseBool xs
parseArg (ArgFile v) xs = liftM v $ parseFile xs
parseArg (ArgNat  v) xs = liftM v $ parseNat  xs
parseArg (ArgInt  v) xs = liftM v $ parseInt  xs
parseArg (ArgPos  v) xs = liftM v $ parsePos  xs


parseNat :: String -> Maybe Int
parseNat x = case parseInt x of
                  Just y | y >= 0 -> Just y
                  _ -> Nothing


parsePos :: String -> Maybe Int
parsePos x = case parseInt x of
                  Just y | y > 0 -> Just y
                  _ -> Nothing


parseInt :: String -> Maybe Int
parseInt x = case reads x of
                  [(a,"")] -> Just a
                  _ -> Nothing


parseFile :: String -> Maybe String
parseFile x = if null x then Nothing else Just x


parseBool :: String -> Maybe Bool
parseBool v | v2 `elem` ["","on","yes","1","true","meep"] = Just True
            | v2 `elem` ["off","no","0","false","moop"] = Just False 
            | otherwise = Nothing
    where v2 = map toLower v



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 726063815

instance Enum CmdFlag
    where toEnum 0 = Version{}
          toEnum 1 = Web{}
          toEnum 2 = Help{}
          toEnum 3 = Test{}
          toEnum 4 = Color{}
          toEnum 5 = Start{}
          toEnum 6 = Count{}
          toEnum 7 = Convert{}
          toEnum 8 = Output{}
          toEnum 9 = Dump{}
          toEnum 10 = DataPath{}
          toEnum 11 = Verbose{}
          toEnum 12 = Info{}
          toEnum n = error ((++) "toEnum " ((++) (show n) ", not defined for CmdFlag"))
          fromEnum (Version {}) = 0
          fromEnum (Web {}) = 1
          fromEnum (Help {}) = 2
          fromEnum (Test {}) = 3
          fromEnum (Color {}) = 4
          fromEnum (Start {}) = 5
          fromEnum (Count {}) = 6
          fromEnum (Convert {}) = 7
          fromEnum (Output {}) = 8
          fromEnum (Dump {}) = 9
          fromEnum (DataPath {}) = 10
          fromEnum (Verbose {}) = 11
          fromEnum (Info {}) = 12
