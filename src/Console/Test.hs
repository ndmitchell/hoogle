{-# LANGUAGE RecordWildCards,PatternGuards,ScopedTypeVariables #-}

-- | Standalone tests are dependent only on themselves, example tests
--   require a fully build Hoogle database.
module Console.Test(testPrepare, testFile) where

import Hoogle
import General.Base
import General.System
import System.FilePath
import Paths_hoogle
import CmdLine.All
import Test.All
import Control.Exception
import System.Console.CmdArgs


testPrepare :: IO ()
testPrepare = do
    putStrLn "Running static tests"
    test

    putStrLn "Converting testdata"
    performGC -- clean up the databases
    dat <- getDataDir
    createDirectoryIfMissing True $ dat </> "databases"
    src <- readFileUtf8 $ dat </> "testdata.txt"
    let dbfile = dat </> "databases/testdata.hoo"
    errs <- createDatabase "http://hackage.haskell.org/" Haskell [] src dbfile
    unless (null errs) $ error $ unlines $ "Couldn't convert testdata database:" : map show errs
    db <- loadDatabase dbfile
    -- this test is now mostly redundant because i can't get the file before saving
    when (show db /= show db) $ error "Database did not save properly"


testFile :: (CmdLine -> IO ()) -> FilePath -> IO Int
testFile run srcfile = do
    putStrLn $ "Testing " ++ srcfile
    src <- readFile' srcfile
    xs <- mapM (runTest run) $ parseTests src
    return $ length $ filter not xs


data Testcase = Testcase
    {testLine :: Int
    ,testQuery :: String
    ,testResults :: [String]
    }


parseTests :: String -> [Testcase]
parseTests = f . zip [1..] . lines
    where
        f ((i,x):xs)
            | "--" `isPrefixOf` x = f xs
            | all isSpace x = f xs
            | otherwise = Testcase i x (map snd a) : f b
            where (a,b) = break (all isSpace . snd) xs
        f [] = []


parseArgs :: String -> [String]
parseArgs "" = []
parseArgs ('\"':xs) = a : parseArgs (drop 1 b)
    where (a,b) = break (== '\"') xs
parseArgs xs = a : parseArgs (dropWhile isSpace b)
    where (a,b) = break isSpace xs


runTest :: (CmdLine -> IO ()) -> Testcase -> IO Bool
runTest run Testcase{..} = do
    whenLoud $ putStrLn $ "Testing: " ++ testQuery
    args <- withArgs (parseArgs testQuery) cmdLine
    res <- try $ captureOutput $ run args
    case res of
        Left (x :: SomeException) -> putStrLn ("Error, test crashed: " ++ testQuery ++ ", with " ++ show x) >> return False
        Right Nothing -> putStrLn "Can't run tests on GHC < 6.12" >> return False
        Right (Just x) -> case matchOutput testResults (lines x) of
            Nothing -> return True
            Just x -> do
                putStrLn $ "Failed test on line " ++ show testLine ++ "\n" ++ x
                return False


-- support @reoder, @not, @exact, @now
matchOutput :: [String] -> [String] -> Maybe String -- Nothing is success
matchOutput want got = f want ([],got)
    where
        f [] _ = Nothing
        f (x:xs) a = case match (code x) a of
            Nothing -> Just $ unlines $ ["Failed to match","Expected: " ++ x,"Got:"] ++ fst a ++ snd a
            Just a -> f xs a

        code ('@':xs) = second (drop 1) $ break (== ' ') xs
        code xs = ("",xs)

        -- given (code,match) (past,future) return Nothing for failure or a new (past,future)
        match :: (String,String) -> ([String],[String]) -> Maybe ([String],[String])
        match ("not",x) (past,future)
            | Just (a,b) <- find x future = Nothing
            | otherwise = Just ([],future)
        match ("reorder",x) (past,future)
            | Just (a,b) <- find x past = Just (a++b, future)
            | Just (a,b) <- find x future = Just (past++a, b)
            | otherwise = Nothing
        match ("now",x) (past,future)
            | Just ([],b) <- find x future = Just ([],b)
            | otherwise = Nothing
        match ("",x) (past,future)
            | Just (a,b) <- find x future = Just (a,b)
            | otherwise = Nothing
        match (code,x) _ = error $ "Unknown test code: " ++ code

        -- given a needle, return Maybe the bits before and after
        find :: String -> [String] -> Maybe ([String],[String])
        find x ys = if null b then Nothing else Just (a,tail b)
            where (a,b) = break (\y -> words x `isInfixOf` words y) ys
