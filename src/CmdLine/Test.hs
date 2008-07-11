
module CmdLine.Test(testFile) where

import Hoogle.All
import General.Code


testFile :: FilePath -> FilePath -> IO ()
testFile srcfile dbfile = do
    putStrLn $ "Testing " ++ srcfile
    db <- loadDataBase dbfile
    src <- readFile srcfile
    let bad = filter (not . runTest db) $ catMaybes $ zipWith parseTest [1..] $ lines src
    if null bad then
        putStrLn "All tests passed"
     else do
        putStr $ unlines $ map failedTest bad
        putStrLn $ show (length bad) ++ " tests failed"


-- LineNo Query Results
data Test = Test Int String [String]
            deriving Show


parseTest :: Int -> String -> Maybe Test
parseTest line str | "@test " `isPrefixOf` str =
    case reads $ drop 5 str of
        [(x,rest)] -> Just $ Test line x (words rest)
        _ -> error $ "Couldn't parse @test on line " ++ show line
parseTest line str = Nothing


runTest :: DataBase -> Test -> Bool
runTest db t = False


failedTest :: Test -> String
failedTest (Test line str _) = "Line " ++ show line ++ ", " ++ str

