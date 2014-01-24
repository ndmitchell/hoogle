
module Console.All(action) where

import CmdLine.All
import Recipe.All
import Recipe.Haddock
import Console.Log
import Console.Search
import Console.Test
import Console.Rank
import General.Base
import General.System
import General.Web
import System.FilePath
import Hoogle
import Hoogle.Type.All


action :: CmdLine -> IO ()

action x@Search{repeat_=i} | i /= 1 = replicateM_ i $ action x{repeat_=1}

action x@Search{queryParsed = Left err} =
    exitMessage ["Parse error:", "  " ++ showTag (parseInput err)
                ,replicate (columnNo err) ' ' ++ " ^"
                ,errorMessage err]
    where showTag = if color x then showTagANSI else showTagText


action (Test files _) = do
    testPrepare
    fails <- fmap sum $ mapM (testFile action) files
    if fails == 0 then putStrLn "Tests passed" else do
        putStrLn $ "TEST FAILURES (" ++ show fails ++ ")"
        exitFailure

action (Rank file) = rank file

action x@Data{} = recipes x

action (Log files) = logFiles files

action (Convert url from to doc merge haddock) = do
    when (any isUpper $ takeBaseName to) $ putStrLn $ "Warning: Hoogle databases should be all lower case, " ++ takeBaseName to
    putStrLn $ "Converting " ++ from
    src <- readFileUtf8 from
    convert url merge (takeBaseName from) to $ unlines $ addLocalDoc doc (lines src)
    where
      addLocalDoc :: Maybe FilePath -> [String] -> [String]
      addLocalDoc doc = if haddock
                          then haddockHacks $ addDoc doc
                          else id

      addDoc :: Maybe FilePath -> Maybe URL
      addDoc = addGhcDoc . fmap (\x -> if "http://" `isPrefixOf` x then x else filePathToURL $ x </> "index.html")

      addGhcDoc :: Maybe URL -> Maybe URL
      addGhcDoc x = if isNothing x && takeBaseName from == "ghc"
                      then Just "http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/"
                      else x


action (Combine from to) = do
    putStrLn $ "Combining " ++ show (length from) ++ " databases"
    mergeDatabase from to

action (Dump file sections) = do
    d <- loadDatabase file
    putStrLn $ "File: " ++ file
    putStr $ showDatabase d $ if null sections then Nothing else Just sections

action q@Search{} | fromRight (queryParsed q) == mempty =
    exitMessage ["No query entered"
                ,"Try --help for command line options"]

action q@Search{} = actionSearch q (fromRight $ queryParsed q)


--- convert a single database
convert :: HackageURL -> [FilePath] -> String -> FilePath -> String -> IO ()
convert url deps x out src = do
    deps2 <- filterM doesFileExist deps
    when (deps /= deps2) $ putStrLn $ "Warning: " ++ x ++ " doesn't know about dependencies on " ++ unwords (deps \\ deps2)
    dbs <- mapM loadDatabase deps2
    putStr $ "Converting " ++ x ++ "... "
    err <- createDatabase url Haskell dbs src out
    putStrLn "done"
    unless (null err) $ putStrLn $ "Skipped " ++ show (length err) ++ " warnings in " ++ x
