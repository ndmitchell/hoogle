
module Main(main) where

import Base; import Keyword; import Hackage; import Default
import Link
import Check
import Util


defaults = ["keyword","hackage","base"
           ,"array","Cabal","HUnit","QuickCheck","bytestring"
           ,"containers","directory","filepath","haskell-src","mtl"
           ,"network","parallel","parsec","pretty","process","random","stm"
           ,"template-haskell","time","xhtml","syb"]
           \\
           ["network","QuickCheck","directory"]



main :: IO ()
main = do
    createDirectoryIfMissing True "temp"
    createDirectoryIfMissing True "result"
    createDirectoryIfMissing True "../../database"
    xs <- getArgs
    xs <- return $ if null xs then defaults else xs
    mapM_ process xs
    link xs
    check


process :: String -> IO ()
process x = do
    putStrLn $ "Processing " ++ x
    case x of
        "base" -> processBase
        "keyword" -> processKeyword
        "hackage" -> processHackage defaults
        _ -> processDefault x
