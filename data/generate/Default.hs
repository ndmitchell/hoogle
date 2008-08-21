
module Default where

import Download
import Haddock
import Hoogle


processDefault x = do
    download x
    haddock x
    hoogle x
