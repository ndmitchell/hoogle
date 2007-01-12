
module Data.Binary.Example where

import Data.Binary.Defer
import System.IO

data Item = Foo Int Bool
          | Blah String
          deriving Show


instance BinaryDefer Item where
    bothDefer = serial
        [\ ~(Foo a b) -> unit Foo << a << b
        ,\ ~(Blah a) -> unit Blah <<~ a
        ]


val = [Foo 1 True, Foo 3 False, Blah "neil ajsklsdafjkl safdkjlfdsajk ladsfjk lafdsjklafsdjkl", Blah "fred", Foo 18 True]

save = do hndl <- openBinaryFile "temp.txt" WriteMode
          putDefer hndl val
          hClose hndl

load = do hndl <- openBinaryFile "temp.txt" ReadMode
          val <- getDefer hndl
          -- hClose hndl
          print (last val :: Item)

