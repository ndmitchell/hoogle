{-# OPTIONS -fplugin=Plugin.HolePlugin -fplugin-opt=Plugin.HolePlugin:600 -funclutter-valid-hole-fits #-}
{-# OPTIONS -fmax-valid-hole-fits=10 #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.PluginDemo where

import Plugin.HolePlugin ()
import Data.Char (toUpper, isUpper)
import Data.Text

test :: Text -> Text 
test x = _turn_around x