
module Hoogle.DataBase.Docs(
    Haddock, loadHaddock,
    Docs, saveDocsHandle, loadDocsHandle, renderDocs
    ) where

import Hoogle.Common.All
import General.All

import System.IO
import System.Directory
import System.FilePath
import Data.List
import Data.Maybe


data Docs = Docs String

data Haddock = Haddock [HtmlTag]

-- load a haddock file
loadHaddock :: FilePath -> [String] -> IO (Maybe Haddock)
loadHaddock haddock modu = do
    let file = haddock </> concat (intersperse "-" modu) <.> "html"
    b <- doesFileExist file
    if not b then return Nothing else do
        src <- readFile file
        return $ Just $ Haddock $ parseHtmlTags src


-- find the documentation in the haddock
-- load it, serialise it to the current position
-- return a bool, were you successful
saveDocsHandle :: Handle -> Haddock -> Item () -> IO Bool
saveDocsHandle handle haddock item =
    case findDocs haddock item of
        Nothing -> return False
        Just x -> return False


findDocs :: Haddock -> Item () -> Maybe Docs
findDocs (Haddock tags) item
    | isItemFunc $ itemRest item = f tags
    | otherwise = Nothing
    where
        f (OpenTag "TR" _ : OpenTag "TD" _ : OpenTag "A" [("NAME",name)] : rest)
            | name == "v%3A" ++ fromJust (itemName item)
            = Just $ Docs $ extractDocs rest
        f (x:xs) = f xs
        f [] = Nothing
        
        
        extractDocs = process . takeWhile (not . isCloseTagName "TD") . drop 1 . dropWhile (not . isOpenTagName "TD")
        
        process x = concatMap g x
        
        g (TextTag x) = x
        g _ = ""


loadDocsHandle :: Handle -> IO Docs
loadDocsHandle _ = return $ Docs "hello neil"



renderDocs :: Docs -> TagStr
renderDocs (Docs x) = Str x

