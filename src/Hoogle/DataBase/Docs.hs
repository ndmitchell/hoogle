
module Hoogle.DataBase.Docs(
    Haddock, loadHaddock,
    Docs, saveDocsHandle, loadDocsHandle, renderDocs
    ) where

import Hoogle.Common.All
import General.All

import System.IO
import System.Directory
import Text.Html(Html,primHtml)
import System.FilePath
import Data.List


data Docs = Docs String

data Haddock = Haddock Html

-- load a haddock file
loadHaddock :: FilePath -> [String] -> IO (Maybe Haddock)
loadHaddock haddock modu = do
    let file = haddock </> concat (intersperse "-" modu) <.> "html"
    b <- doesFileExist file
    if not b then return Nothing else do
        src <- readFile file
        return $ Just $ Haddock $ primHtml src


-- find the documentation in the haddock
-- load it, serialise it to the current position
-- return a bool, were you successful
saveDocsHandle :: Handle -> Haddock -> Item () -> IO Bool
saveDocsHandle _ _ _ = return False



loadDocsHandle :: Handle -> IO Docs
loadDocsHandle _ = return $ Docs "hello neil"



renderDocs :: Docs -> TagStr
renderDocs (Docs x) = Str x

