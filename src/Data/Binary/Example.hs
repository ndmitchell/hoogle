
module Data.Binary.Example where

import Data.Binary.Defer
import System.IO

data Item = Foo Int Bool
          | Blah String
          deriving Show


instance DataFile Item where
    dataFile = serial
        [\ ~(Foo a b) -> unit Foo << a << b
        ,\ ~(Blah a) -> unit Blah <<~ a
        ]


val = [Foo 1 True, Foo 3 False, Blah "neil ajsklsdafjkl safdkjlfdsajk ladsfjk lafdsjklafsdjkl", Blah "fred", Foo 18 True]

save = do hndl <- openBinaryFile "temp.txt" WriteMode
          dataWrite hndl val
          hClose hndl

load = do hndl <- openBinaryFile "temp.txt" ReadMode
          val <- dataRead hndl
          -- hClose hndl
          print (last val :: Item)

