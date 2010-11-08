
module Console.Test(testFile) where

import Hoogle
import General.Code


testFile :: FilePath -> IO ()
testFile srcfile = do
    putStrLn $ "Testing " ++ srcfile
    (dbfile,h) <- openTempFile "." (srcfile <.> "hoo")
    hClose h
    src <- readFile srcfile
    let (errs, dbOld) = createDatabase Haskell [] src
    unless (null errs) $ error $ unlines $ "Couldn't convert database:" : map show errs
    saveDatabase dbfile dbOld
    db <- loadDatabase dbfile
    when (show dbOld /= show db) $ error "Database did not save properly"

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
        [(x,rest)] -> case parseQuery Haskell x of
            Right q -> let (no,yes) = partition ("!" `isPrefixOf`) $ words rest
                       in Just $ Test line x q (map tail no) (map (split ',') yes)
            _ -> err
        _ -> err
    where err = error $ "Couldn't parse @test on line " ++ show line
parseTest line str = Nothing


runTest :: Database -> Test -> Bool
runTest db (Test _ _ q bad ans) =
        ordered (group $ map snd items) &&               -- all results are in order
        all (`elem` map fst items) (concat ans) &&       -- all items are present
        ordered (map (map (`lookupJust` items)) ans) &&  -- all items are in order
        all (`notElem` map fst items) bad                -- all the bad items are absent
    where
        items = map (name .  showTagText . snd . self . snd &&& fst) $ searchAll db q

        ordered ((x:xs):(y:ys):zs) = x < y && all (== x) xs && ordered ((y:ys):zs)
        ordered [x:xs] = all (== x) xs
        ordered [] = True

        name = head . dropWhile (`elem` words "package") . words


failedTest :: Test -> String
failedTest (Test line str _ _ _) = "Line " ++ show line ++ ", " ++ str

