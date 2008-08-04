
module Hoogle.Operations.Rank(rank) where

import General.Code
import Hoogle.TextBase.All
import Hoogle.DataBase.All


rank :: FilePath -> IO ()
rank file = do
    exitMessage ["Todo: Rank on file, " ++ file]
