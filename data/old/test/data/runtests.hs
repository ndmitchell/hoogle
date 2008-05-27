
module Main where

import System
import Directory
import List

main = do x <- getArgs
          if null x
             then error "Expected name of data to test against, i.e. hadhtml, hihoo"
             else exec (head x)


exec :: String -> IO ()
exec x = do tests <- gatherTests
            mapM_ (clearTest x) tests
            putStrLn "== Generating documentation =="
            mapM_ (runTest x) tests
            putStrLn "== Checking generated docs =="
            mapM_ (checkTest x) tests


genHooName mode test = mode ++ "/" ++ test ++ ".hoo"

origHooName test = genHooName "examples" test




gatherTests :: IO [String]
gatherTests = do xs <- getDirectoryContents "examples"
                 return $ [take (length x - 3) x | x <- xs, ".hs" `isSuffixOf` x]


clearTest :: String -> String -> IO ()
clearTest mode test = do b <- doesFileExist file
                         if b then removeFile file else return ()
    where file = genHooName mode test


isWindows :: IO Bool
isWindows = do x <- getEnv "PATH"
               return $ null x || head x /= '/'


command x = do res <- isWindows
               return $ if res
                  then x ++ ".bat"
                  else "./" ++ x ++ ".sh"


runTest :: String -> String -> IO ()
runTest mode test = do cmd <- command ("gen-" ++ mode)
                       system $ cmd ++ " " ++ test
                       return ()


checkTest :: String -> String -> IO ()
checkTest mode test = do gen <- readFile $ genHooName mode test
                         orig <- readFile $ origHooName test
                         let res = compareHoo (lines gen) (lines orig)
                         if null res
                            then putStrLn $ "Passed (" ++ test ++ ")"
                            else putStrLn $ "FAILED " ++ show (length res) ++
                                            " (" ++ test ++ ")\n" ++
                                            unlines res
                                    

compareHoo :: [String] -> [String] -> [String]
compareHoo a b = map ('+':) (diff eqLine a2 b2) ++ map ('-':) (diff eqLine b2 a2)
    where
        a2 = map saneLine $ filter usefulLine a
        b2 = map saneLine $ filter usefulLine b


-- list out those items in the first list that are not in the second list
-- do not allow entries to be reused from the second list
diff :: (a -> a -> Bool) -> [a] -> [a] -> [a]
diff eq a b = f a [] b
    where
        f [] _ _ = []
        f (a:as) done (b:bs) | eq a b = f as [] (done ++ bs)
                             | otherwise = f (a:as) (b:done) bs
        f (a:as) done [] = a : f as [] done


usefulLine ('-':'-':_) = False
usefulLine [] = False
usefulLine _ = True


saneLine (' ':' ':xs) = saneLine (' ':xs)
saneLine [' '] = []
saneLine (x:xs) = x : saneLine xs
saneLine [] = []


eqLine :: String -> String -> Bool
eqLine a b = a == b
