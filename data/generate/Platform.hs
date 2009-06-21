
module Platform where

import Util

-- Exclude: Cabal GLUT Win32 unix OpenGL

libs = words $
    "array base bytestring containers directory filepath ghc-prim haskell98 " ++
    "hpc integer old-locale old-time packedstring pretty random syb template-haskell " ++
    "cgi fgl editline haskell-src html HUnit mtl network parallel parsec " ++
    "QuickCheck regex-base regex-compat regex-posix stm time xhtml zlib HTTP hackage"

bad = words "ghc-prim haskell98 integer old-locale packedstring"

platform = hoogle_ $ hooFlag "output" "default" : [hooFlag "combine" x | x <- libs, x `notElem` bad]
