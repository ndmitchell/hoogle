
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
import Data.Char
import Control.Monad


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
saveDocsHandle hndl haddock item =
    case findDocs haddock item of
        Nothing -> return False
        Just (Docs x) -> hPutString hndl x >> return True


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
        
        process = g False
        
        g pre (TextTag x : xs) = (if pre then id else remSpaces) x ++ g pre xs
        g pre (OpenTag "PRE" _ : xs) = g True xs
        g pre (CloseTag "PRE" : xs) = g False xs
        g pre (CloseTag "P" : xs) = "\n\n" ++ g pre xs
        g pre (x:xs) = g pre xs
        g pre [] = []
        
        remSpaces (x:y:xs) | isSpace x && isSpace y = remSpaces (' ':xs)
        remSpaces (x:xs) = (if isSpace x then ' ' else x) : remSpaces xs
        remSpaces [] = []


loadDocsHandle :: Handle -> IO Docs
loadDocsHandle hndl = liftM Docs $ hGetString hndl


renderDocs :: Docs -> TagStr
renderDocs (Docs x) = Str x

