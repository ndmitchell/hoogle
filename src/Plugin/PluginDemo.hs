{-# OPTIONS -fplugin=Plugin.HolePlugin -fplugin-opt=Plugin.HolePlugin:600 -funclutter-valid-hole-fits #-}
module Plugin.PluginDemo (hello) where

import Plugin.HolePlugin

test :: Int -> Int 
test x = _ x