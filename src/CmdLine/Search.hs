
module CmdLine.Search(actionSearch) where

import CmdLine.Flag
import Control.Monad
import Data.Maybe
import Data.List
import Data.Range
import General.All
import Hoogle.Query.All
import Hoogle.Search.All
import Hoogle.DataBase.All
import Hoogle.All
import Safe
import System.Directory
import System.FilePath


actionSearch :: [CmdFlag] -> Query -> IO ()
actionSearch flags q = do
    db <- getDataBases flags q
    when verbose $
        putStr $ unlines $ "= DATABASES =" : map ("  "++) db

    dbs <- mapM loadDataBase db
    let sug = suggestQuery dbs q
    when (isJust sug) $
        putStrLn $ showTag $ fromJust sug

    when verbose $ putStrLn "= ANSWERS ="
    let res = search dbs q
    if null res
        then putStrLn "No results found"
        else putStr $ unlines $ map (f . renderResult) res
    where
        search | null start && null count = searchAll
               | otherwise = let s = headDef 0 start
                                 n = headDef maxBound count
                             in searchRange (rangeStartCount s n)
            where start = [i-1 | Start i <- flags]
                  count = [i | Count i <- flags]

        verbose = Verbose `elem` flags
        showTag = if Color True `elem` flags then showTagConsole else show

        f (m,r,v) = maybe "" (\m -> showModule m ++ " ") m ++
                    showTag r ++ (if verbose then " " ++ v else "")


---------------------------------------------------------------------
-- Pick the DataBase's

-- pick "default" if there are not ones specified
-- otherwise use the CmdFlag and any +package query flags
-- 

getDataBases :: [CmdFlag] -> Query -> IO [FilePath]
getDataBases flags q = do
        (files,dirs) <- dataFileDir flags
        let pkg1 = [x | PlusPackage x <- scope q]
            pkg2 = if null pkg1 && null files then ["default"] else pkg1
        files2 <- mapM (resolveName dirs) pkg2
        return $ nub $ files++files2


-- return the (files,dirs), error message if one does not exist
dataFileDir :: [CmdFlag] -> IO ([FilePath],[FilePath])
dataFileDir flags = f [x | DataPath x <- flags]
    where
        f [] = return ([],[])
        f (x:xs) = do
            ft <- fileType x
            (x,ft) <- if (ft == NotFound && not (hasExtension x))
                      then do x <- return $ x <.> "hoo"
                              liftM ((,) x) (fileType x)
                      else return (x,ft)

            when (ft == NotFound) $
                failMessage ["File given with the --data flag does not exist"
                            ,"    " ++ x]
            (fs,ds) <- f xs
            return $ if ft == File then (x:fs,ds) else (fs,x:ds)


-- change PlusPackage flags from names to files
resolveName :: [FilePath] -> String -> IO FilePath
resolveName dirs x = f (dirs ++ ["."])
    where
        f [] = failMessage ["DataBase not found","    " ++ x] >> return ""
        f (d:ds) = do
            let s = d </> x <.> "hoo"
            b <- doesFileExist s
            if b then return s else f ds
