
module Test.Parsing(mainParsing) where

import Text.ParserCombinators.Parsec
import Hoogle.TypeSig.All
import Data.List
import Data.Char


mainParsing = testParser parseTypeSig "TypeSig/tests.txt"


testParser :: (Eq a, Show a, Read a) => (String -> Either ParseError a) -> FilePath -> IO ()
testParser f file = do
        src <- readFile $ "../Hoogle/" ++ file
        sequence_ $ zipWith g [1..] $ lines src
    where
        g n l | "--" `isPrefixOf` l || all isSpace l = return ()
        g n l = case f a of
                    Left x -> error $ msg ++ show x
                    Right x -> if x == read b then return () else
                               error $ msg ++ show x ++ " vs " ++ b
            where
                (a,_:b) = break (== '£') l
                msg = "Parse test failed, " ++ file ++ ", " ++ show n ++ ": "
