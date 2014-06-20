{-# LANGUAGE RecordWildCards #-}

module Console.Search(actionSearch) where

import CmdLine.All
import General.Base
import General.System
import System.Console.CmdArgs
import Hoogle


actionSearch :: CmdLine -> Query -> IO ()
actionSearch flags q = do
    let dir = last $ "." : databases flags
    let sug = suggestions dir q
    when (isJust sug) $
        putStrLn $ showTag $ fromJust sug
    verbose <- isLoud
    when verbose $ putStrLn "= ANSWERS ="

    when (color flags) $
        putStrLn $ "Searching for: " ++ showTag (renderQuery q)

    res <- restrict . concatMap expand . map ((,) ()) <$> search dir q
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
