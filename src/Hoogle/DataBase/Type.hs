
module Hoogle.DataBase.Type(
    DataBase(..), createDataBase,
    locateWebDocs
    ) where


import Hoogle.DataBase.Items
import Hoogle.DataBase.Modules
import Hoogle.DataBase.Texts
import Hoogle.Item.All

import Data.List
import General.All


data DataBase = DataBase {
                    package :: String,
                    webdocs :: String,
                    
                    -- the static and cached information
                    items :: Items,
                    modules :: Modules,
                    texts :: Texts
                }



createDataBase :: [Item] -> DataBase
createDataBase items1 = DataBase "" "" newItems newModules (createTexts items3)
    where
        -- these are the two things that change the items
        (items2,newModules) = createModules items1
        (items3,newItems  ) = createItems items2


{-
-- forward methods
searchName :: DataBase -> [String] -> IO [Result DataBase]
searchName database query = do
    let hndl = handle database
    res <- mapM (\x -> hSetPos hndl (nameSearchPos database) >> searchTexts hndl x) query
    if length query == 1
        then mapM f $ head res
        else mergeResults database query res
    where
        f = liftM (fixupTextMatch (head query)) . loadResult . setResultDataBase database


mergeResults :: DataBase -> [String] -> [[Result ()]] -> IO [Result DataBase]
mergeResults database query xs = merge (map (sortBy cmp) xs)
    where
        getId = fromJust . itemId . itemResult
        cmp a b = getId a `compare` getId b

        merge xs | null tops = return []
                 | otherwise = do
                 result <- loadResult $ setResultDataBase database $ snd $ head nows
                 let tm = computeTextMatch
                                    (fromJust $ itemName $ itemResult result)
                                    [(q, head $ textMatch $ fromJust $ textResult x) | (q,x) <- nows]
                     result2 = result{textResult = Just tm}
                 ans <- merge rest
                 return (result2:ans)
            where
                nows = [(q,x) | (q,x:_) <- zip query xs, getId x == now]
                rest = [if not (null x) && getId (head x) == now then tail x else x | x <- xs]
                now = minimum $ map getId tops
                tops = [head x | x <- xs, not $ null x]


-- fill in the global methods of TextMatch
-- given a list of TextMatchOne
fixupTextMatch :: String -> Result a -> Result a
fixupTextMatch query result = result{textResult = Just newTextResult}
    where
        name = fromJust $ itemName $ itemResult result
        matches = textMatch $ fromJust $ textResult result
        newTextResult = computeTextMatch name [(query,head matches)]
-}


locateWebDocs :: DataBase -> Item -> Maybe String
locateWebDocs db item
    | null url = Nothing
    | isItemModule $ itemRest item = Just $ url ++ modu ++ ['-'|not $ null modu] ++ itemName item ++ ".html"
    | otherwise = Just $ url ++ modu ++ ".html#" ++ code ++ nam
    where
        nam = escape $ itemName item
        modu = concat $ intersperse "-" $ modName $ itemMod item
        url = webdocs db
        
        code = case itemRest item of
                    ItemFunc{} -> "v%3A"
                    _ -> "t%3A"
