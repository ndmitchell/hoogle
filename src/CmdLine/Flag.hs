{-|
    Parse a list of flags supported in any of the front ends.

    Returns the flags in a data structure, along with any invalid flags.
    
    Deal with any files/include files in this stage.
-}

module CmdLine.Flag(
    CmdFlag(..), flagsHelp,
    flagsWebArgs, flagsWebQuery, flagsCmdLine
    ) where

import General.Code
import General.Glob

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
             | Dump String       -- ^ Dump a database to a file (optional section)
             | DataFile FilePath -- ^ Database location
             | Verbose           -- ^ Display verbose information
             | Info              -- ^ Display as much information as you can
             | Debug             -- ^ Do debugging activities
             | Include FilePath  -- ^ Include directory
             | TestFile FilePath -- ^ Run tests in a file
             | Rank FilePath     -- ^ Generate rankings
             | Combine FilePath  -- ^ Merge a set of databases
             | Mode String       -- ^ Web modes
               deriving (Show,Eq {-! Enum !-} )


-- | In which circumstances are you allowed to pass this command
data Permission = PWebArgs | PWebQuery | PCmdLine | PMultiple
                  deriving (Show,Eq)

data Argument = ArgNone CmdFlag
              | ArgBool (Bool     -> CmdFlag)
              | ArgInt  (Int      -> CmdFlag)
              | ArgNat  (Int      -> CmdFlag)
              | ArgPos  (Int      -> CmdFlag)
              | ArgFileIn (FilePath -> CmdFlag) [String]
              | ArgFileOut (FilePath -> CmdFlag)
              | ArgDir (FilePath -> CmdFlag)
              | ArgStr  (String   -> CmdFlag)

instance Show Argument where
    show (ArgNone _) = ""
    show (ArgInt  _) = "INT"
    show (ArgNat  _) = "NAT"
    show (ArgPos  _) = "POS"
    show (ArgBool _) = "BOOL"
    show (ArgFileIn _ _) = "FILE"
    show (ArgFileOut _) = "FILE"
    show (ArgDir _) = "DIR"
    show (ArgStr  _) = "STR"


data FlagInfo = FlagInfo {
    argument :: Argument,
    names :: [String],
    permissions :: [Permission],
    description :: String
    }

flagInfo =
    [f (ArgNone Version) ["version","ver"] [PCmdLine] "Print out version information"
    ,f (ArgNone Help) ["?","help","h"] [PCmdLine] "Show help message"
    ,f (ArgNone Web) ["w","web"] [PCmdLine] "Run as though it was a CGI script"
    ,f (ArgBool Color) ["c","color","col","colour"] [PCmdLine] "Show color output (default=false)"
    ,f (ArgPos  Start) ["s","start"] [PCmdLine,PWebArgs] "First result to show (default=1)"
    ,f (ArgPos  Count) ["n","count","length","len"] [PCmdLine,PWebArgs] "Number of results to show (default=all)"
    ,f (ArgNone Test) ["test"] [PCmdLine] "Run the regression tests"
    ,f (ArgFileIn Convert ["txt"]) ["convert"] [PCmdLine,PMultiple] "Convert a database"
    ,f (ArgFileOut Output) ["output"] [PCmdLine] "Output file for convert"
    ,f (ArgStr  Dump) ["dump"] [PCmdLine] "Dump a database for debugging"
    ,f (ArgFileIn DataFile ["hoo"]) ["d","data"] [PCmdLine,PMultiple] "Database file"
    ,f (ArgNone Verbose) ["v","verbose"] [PCmdLine] "Display verbose information"
    ,f (ArgNone Info) ["info"] [PCmdLine] "Display full information on an entry"
    ,f (ArgNone Debug) ["debug"] [PCmdLine] "Debugging only"
    ,f (ArgDir Include) ["i","include"] [PCmdLine,PMultiple] "Include directories"
    ,f (ArgFileIn TestFile ["txt"]) ["testfile"] [PCmdLine,PMultiple] "Run tests from a file"
    ,f (ArgFileIn Rank ["txt"]) ["rank"] [PCmdLine,PMultiple] "Generate ranking scores"
    ,f (ArgFileIn Combine ["hoo"]) ["combine"] [PCmdLine,PMultiple] "Combine multiple databases"
    ,f (ArgStr Mode) ["mode"] [PCmdLine,PWebArgs] "Web mode"
    ]
    where f = FlagInfo


cmdFlagBadArg flag typ "" = "Missing argument to flag " ++ flag ++ ", expected argument of type " ++ typ
cmdFlagBadArg flag "" x = "Unexpected argument to flag " ++ flag ++ ", got \"" ++ x ++ "\""
cmdFlagBadArg flag typ x = "Bad argument to flag " ++ flag ++ ", expected argument of type " ++ typ ++ ", got \"" ++ x ++ "\""

cmdFlagPermission flag = "Flag not allowed when running in this mode, flag " ++ flag

cmdFlagUnknown flag = "Unknown flag " ++ flag

cmdFlagDuplicate flag = "The flag " ++ flag ++ " may only occur once, but occured multiple times"


---------------------------------------------------------------------
-- Operations on Flags

-- | flags that are passed in through web arguments,
--   i.e. ?foo=bar&...
flagsWebArgs :: [(String,String)] -> IO ([CmdFlag],[String])
flagsWebArgs = parseFlags PWebArgs


-- | flags that are given in the web query string
flagsWebQuery :: [(String,String)] -> IO ([CmdFlag],[String])
flagsWebQuery = parseFlags PWebQuery


-- | flags that are given in a query on the command line
flagsCmdLine :: [(String,String)] -> IO ([CmdFlag],[String])
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

        typ x = ['='|s/=""] ++ s
            where s = show x


---------------------------------------------------------------------
-- Parsing Flags

-- TODO: check no flag is specified twice
-- TODO: Fix a bug, try /merge=t1;t2 and you get [t1,t1,t2,t2]
parseFlags :: Permission -> [(String,String)] -> IO ([CmdFlag],[String])
parseFlags perm xs = do
    let args = concatMap (parseFlag perm) xs
        inc = [x | Right (_,_,_,Include x) <- args]
    incs <- mapM globDir $ ["."|null inc] ++ inc
    (a,b) <- mapAndUnzipM (f incs) args
    return ([Include "."|null inc] ++ concat a, concat b)
    where
        f inc (Right (_,val,FlagInfo{argument=ArgFileIn gen exts},_)) = do
            let vals = parseFile val
            files <- concatMapM (globFile inc exts) vals
            return (map gen files, [])
        f inc (Left v) = return ([],[v])
        f inc (Right (_,_,_,v)) = return ([v],[])


-- does all validity checks apart from checking for duplicate flags
parseFlag :: Permission -> (String, String) -> [Either String (String,String,FlagInfo,CmdFlag)]
parseFlag perm (key,val)
        | isNothing m = [Left $ cmdFlagUnknown key]
        | perm `notElem` permissions flg = [Left $ cmdFlagPermission key]
        | null arg = [Left $ cmdFlagBadArg key (show $ argument flg) val]
        | otherwise = [Right (key,val,flg,a) | a <- arg]
    where
        key2 = lower key
        m@ ~(Just flg) = listToMaybe [i | i <- flagInfo, key2 `elem` names i]
        arg = parseArg (argument flg) val


parseArg :: Argument -> String -> [CmdFlag]
parseArg (ArgNone v) xs = [v | null xs]
parseArg (ArgStr  v) xs = [v xs]
parseArg (ArgBool v) xs = map v $ parseBool xs
parseArg (ArgNat  v) xs = map v $ parseNat  xs
parseArg (ArgInt  v) xs = map v $ parseInt  xs
parseArg (ArgPos  v) xs = map v $ parsePos  xs
parseArg (ArgFileIn v _) xs = map v $ parseFile xs
parseArg (ArgFileOut v) xs = map v $ parseFile xs
parseArg (ArgDir v) xs = map v $ parseFile xs


parseNat, parsePos, parseInt :: String -> [Int]
parseNat = filter (>= 0) . parseInt
parsePos = filter (> 0) . parseInt
parseInt x = [a | (a,"") <- reads x]

parseFile :: String -> [String]
parseFile = splitSearchPath

parseBool :: String -> [Bool]
parseBool v | v2 `elem` ["","on","yes","1","true","meep"] = [True]
            | v2 `elem` ["off","no","0","false","moop"] = [False]
            | otherwise = []
    where v2 = lower v



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 1401092643

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
          toEnum 10 = DataFile{}
          toEnum 11 = Verbose{}
          toEnum 12 = Info{}
          toEnum 13 = Debug{}
          toEnum 14 = Include{}
          toEnum 15 = TestFile{}
          toEnum 16 = Rank{}
          toEnum 17 = Combine{}
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
          fromEnum (DataFile {}) = 10
          fromEnum (Verbose {}) = 11
          fromEnum (Info {}) = 12
          fromEnum (Debug {}) = 13
          fromEnum (Include {}) = 14
          fromEnum (TestFile {}) = 15
          fromEnum (Rank {}) = 16
          fromEnum (Combine {}) = 17
