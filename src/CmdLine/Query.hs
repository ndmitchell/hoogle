
module CmdLine.Query(cmdQuery) where

import System.Environment
import Data.List
import Data.Char


cmdQuery :: IO String
cmdQuery = do s <- getArgs
              return $ concat $ intersperse " " $ map f s
    where
        f x = q ++ x ++ q
            where q = ['\"' | any isSpace x]