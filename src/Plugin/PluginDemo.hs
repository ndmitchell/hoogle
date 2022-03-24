{-# OPTIONS -fplugin=Plugin.HolePlugin -fplugin-opt=Plugin.HolePlugin:600 -funclutter-valid-hole-fits #-}
{-# OPTIONS -fmax-valid-hole-fits=10 #-}
module Plugin.PluginDemo where

import Plugin.HolePlugin

test :: Int -> Int 
test x = _ x