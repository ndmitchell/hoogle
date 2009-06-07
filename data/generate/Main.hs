
module Main(main) where

import Util
import Packages
import Process
import Keywords
import Legacy
import Base
import Link
import Platform


legacy = words "array bytestring containers pretty stm template-haskell syb"

exclude = let a++b = concat[a," ",b] in words $
    -- bug #184
    "HList TypeCompose typical unicode-prelude uvector applicative-extras IOSpec" ++
    "vector-space category-extras checkers reactive strict Vec linear-maps llvm" ++
    "queuelike rewriting emgm grapefruit-records multirec RepLib sessions" ++
    "HAppS-State happstack-state test-framework darcs" ++
    -- implicit parameters
    "ieee-utils line2pdf encoding" ++
    -- type equality ~
    "data-reify tfp" ++
    -- weirdness with conv[foo]
    "harpy HAppS-Data happstack-data" ++
    -- parsing bugs
    "haskell-src-meta"


main :: IO ()
main = do
    createDirectoryIfMissing True "temp"
    createDirectoryIfMissing True "result"
    createDirectoryIfMissing True "../../database"
    xs <- packages
    mapM_ f xs
    keywords
    xs <- getDirectoryContents "result"
    link [a | x <- xs, let (a,b) = splitExtension x, b == ".txt", a `notElem` exclude]
    platform


f pkg@(name,ver)
    | name == "base" = processBase pkg
    | name `elem` legacy = processLegacy pkg
    | otherwise = process pkg
