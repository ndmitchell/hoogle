{-# LANGUAGE RecordWildCards #-}

module Console.Search(actionSearch) where

import CmdLine.All
import General.Base
import General.System
import System.Console.CmdArgs
import Hoogle


actionSearch :: CmdLine -> Query -> IO ()
actionSearch flags q = do
    (missing,dbs) <- loadQueryDatabases (databases flags) q
    unless (null missing) $ do
        n <- availableDatabases (databases flags)
        exitMessage $
            ("Could not find some databases: " ++ unwords missing) :
            "Searching in:" : map ("  "++) (databases flags) ++ [""] ++
            (if null n then ["There are no available databases, generate them with: hoogle data"]
             else ["Either the package does not exist or has not been generated"] ++
                  ["Generate more databases with: hoogle data all" | length n < 100] ++
                  ["Found " ++ show (length n) ++ " databases, including: " ++ unwords (take 5 n) | not $ null n])

    let sug = suggestions dbs q
    when (isJust sug) $
        putStrLn $ showTag $ fromJust sug
    verbose <- isLoud
    when verbose $ putStrLn "= ANSWERS ="

    when (color flags) $
        putStrLn $ "Searching for: " ++ showTag (renderQuery q)

    let res = restrict $ concatMap expand $ search dbs q
    if null res then
        putStrLn "No results found"
     else if info flags then do
        let Result{..} = snd $ head res
        putStrLns 2 $ disp verbose $ head res
        putStrLns 2 $ showTag docs
        case locations of
            (_,(_,p):_):_ -> putStrLn $ "From package " ++ p
            _ -> return ()
        putStrLns 1 $ showTag self
     else
        putStr $ unlines $ map (disp verbose) res
    where
        restrict | start2 == 0 && count2 == maxBound = id
                 | otherwise = take count2 . drop start2
            where start2 = maybe 0 (subtract 1) $ start flags
                  count2 = fromMaybe maxBound $ count flags

        showTag = if color flags then showTagANSI else showTagText

        expand (s,r) | null $ locations r = [(s,r)]
                     | otherwise = [(s,r{locations=[p]}) | p <- locations r]

        disp verbose (s,Result{..}) =
            (case locations of (_,_:(_,m):_):_ -> m ++ " "; _ -> "") ++
            showTag self ++
            (if verbose then "  -- " ++ show s else "") ++
            (if link flags then " -- " ++ head (map fst locations ++ [""]) else "")


-- Put out a string with some blank links following
-- Do not put out the blank lines if no text output
putStrLns :: Int -> String -> IO ()
putStrLns n xs = when (xs /= "") $ do
                     putStr xs
                     putStr $ replicate n '\n'
