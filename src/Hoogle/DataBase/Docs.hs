
module Hoogle.DataBase.Docs(Docs, saveDocsHandle, loadDocsHandle, renderDocs) where

import Hoogle.Common.All
import General.All
import System.IO


data Docs = Docs String


-- find the documentation in the haddock
-- load it, serialise it to the current position
-- return a bool, were you successful
saveDocsHandle :: FilePath -> Item () -> Handle -> IO Bool
saveDocsHandle _ _ _ = return False



loadDocsHandle :: Handle -> IO Docs
loadDocsHandle _ = return $ Docs "hello neil"



renderDocs :: Docs -> TagStr
renderDocs (Docs x) = Str x

