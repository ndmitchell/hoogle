
module ScrapeModule(scrapeModule) where

import Text.HTML.TagSoup
import Control.Monad.State
import Data.List
import Data.Maybe

pick f = head . filter f


scrapeModule :: String -> [String]
scrapeModule x = modname ++ documentation tags
    where (modname, tags) = moduleName $ parseTags x


moduleName :: [Tag] -> ([String], [Tag])
moduleName xs = (["module " ++ title], rest)
    where _:TagText title:rest = dropWhile (~/= "<TITLE>") xs


documentation :: [Tag] -> [String]
documentation xs = concatMap declaration decls
    where
        docs = pick (\ys -> (ys !! 1) == TagText "Documentation") $ partitions (~== "<TD CLASS=section1>") xs
        decls = partitions (~== "<TD CLASS=s15>") docs


declaration, declType, declData, declClass, declFunc :: [Tag] -> [String]
declaration x = case head $ words $ innerText x of
    "type" -> declType x
    "newtype" -> declData x
    "data" -> declData x
    "class" -> declClass x
    _ -> declFunc x


declType xs = maybe [] markup (lookup "doc" tds) ++ [innerText $ fromJust $ lookup "decl" tds]
    where tds = [(fromAttrib "CLASS" y, ys) | y:ys <- partitions (~== "<TD>") xs]


declData _ = []


declClass _ = []


declFunc _ = []


markup :: [Tag] -> [String]
markup _ = ["-- | Markup here"]
