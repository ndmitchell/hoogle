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


type URL = String

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c


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


ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse
trim = ltrim . rtrim


chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
    where (b, as') = f as
