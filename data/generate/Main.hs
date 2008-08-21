
module Main(main) where

import Base
import Keyword
import Default
import Link
import Util


defaults = ["keyword","base","array","Cabal","HUnit","QuickCheck","bytestring"
           ,"containers","directory","filepath","haskell-src","mtl"
           ,"network","parallel","parsec","pretty","process","random","stm"
           ,"template-haskell","time","xhtml"]
           \\
           ["network"]



main :: IO ()
main = do
    createDirectoryIfMissing True "temp"
    createDirectoryIfMissing True "result"
    createDirectoryIfMissing True "../../database"
    xs <- getArgs
    xs <- return $ if null xs then defaults else xs
    mapM_ process xs
    link xs


process :: String -> IO ()
process x = do
    putStrLn $ "Processing " ++ x
    case x of
        "base" -> processBase x
        "keyword" -> processKeyword x
        _ -> processDefault x
