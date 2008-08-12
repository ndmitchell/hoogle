
module CmdLine.Test(testFile) where

import Hoogle.All
import Hoogle.Item.All
import General.Code
import Data.Binary.Defer.Index


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


-- LineNo Query NoResults YesResults
-- NoResults is a list of results that are not allowed to appear
-- YesResults are sets of results, which must be in order, and within a set must
--            have the same Score
data Test = Test Int String Query [String] [[String]]
            deriving Show


parseTest :: Int -> String -> Maybe Test
parseTest line str | "@test " `isPrefixOf` str =
    case reads $ drop 5 str of
        [(x,rest)] -> case parseQuery x of
            Right q -> let (no,yes) = partition ("!" `isPrefixOf`) $ words rest
                       in Just $ Test line x q (map tail no) (map (split ',') yes)
            _ -> err
        _ -> err
    where err = error $ "Couldn't parse @test on line " ++ show line
parseTest line str = Nothing


runTest :: DataBase -> Test -> Bool
runTest db (Test _ _ q bad ans) =
        ordered (group $ map resultScore res) &&         -- all results are in order
        all (`elem` map fst items) (concat ans) &&       -- all items are present
        ordered (map (map (`lookupJust` items)) ans) &&  -- all items are in order
        all (`notElem` map fst items) bad                -- all the bad items are absent
    where
        res = searchAll [db] q
        items = map (entryName . fromLink . resultEntry &&& resultScore) res

        ordered ((x:xs):(y:ys):zs) = x < y && all (== x) xs && ordered ((y:ys):zs)
        ordered [x:xs] = all (== x) xs
        ordered [] = True


failedTest :: Test -> String
failedTest (Test line str _ _ _) = "Line " ++ show line ++ ", " ++ str

