-- Check for random problems that have been seen in the past

module Check(check) where

import Data.List
import qualified Data.Map as Map


check :: IO ()
check = do
    putStrLn "Checking for known errors..."
    src <- readFile "result/base.txt"
    let errs = checkBase $ lines src
    if null errs
        then putStrLn "None found"
        else putStrLn $ unlines errs ++ "ERRORS FOUND"


-- Problems to check for:
-- want at least "even ::", "tan :: ", "cos ::", "log ::"
-- want module Prelude exactly once
checkBase :: [String] -> [String]
checkBase = check . foldl' add db
    where
        db = Map.fromList $ map (flip (,) 0) $ tests
        tests = ["module","Prelude"] : map (\x -> [x,"::"]) funcs
        funcs = ["even","tan","cos","log","sin","seq"]

        add db x = Map.adjustWithKey (const (+1)) (take 2 $ words x) db
        
        check db = [unwords $ "Found" : key ++ [show val] | (key,val) <- Map.toList db, val /= 1]
