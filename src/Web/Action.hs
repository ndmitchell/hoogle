
module Web.Action(actionWeb) where

import CmdLine.All
import Hoogle.All
import Hoogle.Query.All
import General.Code
import System.IO.Unsafe(unsafeInterleaveIO)


actionWeb :: CmdQuery -> IO ()
actionWeb q = do
    (skipped,dbs) <- loadDataBases q
    let res = runQuery q dbs
    when (Debug `elem` queryFlags q) $
        writeFile "temp.htm" res
    putStrLn res


-- is the package not something that might go wrong
safePackage :: String -> Bool
safePackage = all $ \x -> isAlphaNum x || x `elem` "-_"


-- return the databases you loaded, and those you can't
-- guarantees not to actually load the databases unless necessary
-- TODO: Should say which databases are ignored
loadDataBases :: CmdQuery -> IO ([String], [DataBase])
loadDataBases CmdQuery{query=Right q} = do
    let pkgs = nub [x | PlusPackage x <- scope q, safePackage x]
        files = if null pkgs then ["default"] else pkgs
    files <- filterM doesFileExist $ map (\x -> "res" </> x <.> "hoo") files
    dbs <- unsafeInterleaveIO $ mapM loadDataBase files
    return ([], dbs)
loadDataBases _ = return ([], [])


runQuery :: CmdQuery -> [DataBase] -> String
runQuery _ _ = "hello"
