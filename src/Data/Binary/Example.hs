
module Data.Binary.Example where

import Data.Binary.Defer
import System.IO

data Item = Foo Int Bool
          | Blah String
          deriving Show


instance BinaryDefer Item where
    bothDefer = defers
        [\ ~(Foo a b) -> unit Foo << a << b
        ,\ ~(Blah a) -> unit Blah <<~ a
        ]


val = [Foo 1 True, Blah "neil ajsklsdafjkl safdkjlfdsajk ladsfjk lafdsjklafsdjkl", Blah "fred", Foo 18 True]

save = do hndl <- openBinaryFile "temp.txt" WriteMode
          put hndl val
          hClose hndl

load = do hndl <- openBinaryFile "temp.txt" ReadMode
          val <- get hndl
          -- hClose hndl
          print (val :: [Item])

