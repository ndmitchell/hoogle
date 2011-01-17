{-# LANGUAGE CPP #-}

-- | Module for "pure" things in the base, and things I think should
--   have been in base, or could plausibly be added.
module General.Base(module General.Base, module X) where

import Control.Arrow as X
import Control.Monad as X
import Data.Char as X
import Data.Data as X (Data,Typeable)
import Data.Either as X
import Data.Function as X
import Data.List as X
import Data.Maybe as X
import Data.Monoid as X
import Data.Ord as X
import Debug.Trace as X (trace)
import Numeric as X (readHex,showHex)
import System.FilePath as X hiding (combine)

import Control.Exception(bracket)
import System.IO


-- | A URL, or internet address. These addresses will usually start with either
--   @http:\/\/@ or @file:\/\/@.
type URL = String

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c


swap (a,b) = (b,a)

fromLeft (Left x) = x
fromRight (Right x) = x

isLeft Left{} = True; isLeft _ = False
isRight Right{} = True; isRight _ = False


concatMapM f = liftM concat . mapM f


unzipEithers :: [Either a b] -> ([a],[b])
unzipEithers [] = ([],[])
unzipEithers (Left x:xs) = (x:a,b)
    where (a,b) = unzipEithers xs
unzipEithers (Right x:xs) = (a,x:b)
    where (a,b) = unzipEithers xs


initLast :: [a] -> ([a], a)
initLast [] = error "initLast, empty list []"
initLast [x] = ([], x)
initLast (x:xs) = (x:a, b)
    where (a,b) = initLast xs


lower = map toLower
upper = map toUpper


readFile' x = do
    src <- readFile x
    length src `seq` return src


readFileUtf8' :: FilePath -> IO String
readFileUtf8' x = do
    src <- readFileUtf8 x
    length src `seq` return src


readFileUtf8 :: FilePath -> IO String
#if __GLASGOW_HASKELL__ < 612
readFileUtf8 x = readFile x
#else
readFileUtf8 x = do
    h <- openFile x ReadMode
    hSetEncoding h utf8
    hGetContents h
#endif


writeFileUtf8 :: FilePath -> String -> IO ()
#if __GLASGOW_HASKELL__ < 612
writeFileUtf8 x y = writeFile x y
#else
writeFileUtf8 x y = bracket (openFile x WriteMode) hClose $ \h -> do
    hSetEncoding h utf8
    hPutStr h y
#endif


ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse
trim = ltrim . rtrim


chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
    where (b, as') = f as


fromList :: a -> [a] -> a
fromList x [] = x
fromList x (y:ys) = y
