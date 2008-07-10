
module General.Glob(globFile, globDir) where

import General.Code


-- Find the file given directories, extensions, search
-- supports * glob key
globFile :: [FilePath] -> [String] -> FilePath -> IO [FilePath]
globFile dirs exts file = f poss
    where
        f [] = exitMessage $ ["Could not find file:"
                             , "    " ++ file
                             ,"Searched:"] ++
                             map ("    " ++) poss
        f (x:xs) = do
            y <- globFileTest x
            if null y then f xs else return y

        exts2 = "" : if hasExtension file then [] else exts
        poss = [d </> file <.> e | d <- dirs, e <- exts2]


globFileTest :: FilePath -> IO [FilePath]
globFileTest file = do
    (dir,fil) <- return $ splitFileName file
    if '*' `notElem` fil then do
        b <- doesFileExist file
        return [file | b]
     else do
        s <- getDirectoryContents dir
        concatMapM (f dir fil) s
    where
        f dir fil x = do
            let dirx = dir </> x
            b <- doesFileExist dirx
            return [dirx | b && globMatch fil x]


globMatch :: String -> String -> Bool
globMatch glob s | null glob = False
                 | otherwise =
        head matches `isPrefixOf` s2 && last matches `isSuffixOf` s2 && f matches s2
    where
        norm = if searchPathSeparator == ';' then lower else id
        s2 = norm s
        matches = split '*' $ norm glob

        f []     s  = True
        f (m:ms) [] = False
        f (m:ms) s  | m `isPrefixOf` s = f ms $ drop (length m) s
                    | otherwise = f (m:ms) (tail s)


-- check for existence and crash if not
globDir :: FilePath -> IO FilePath
globDir x = do
    b <- doesDirectoryExist x
    if b then return x else
        exitMessage ["Could not find directory:"
                    , "    " ++ x]
