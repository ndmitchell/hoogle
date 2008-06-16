
module Hoogle.DataBase.All where

import Hoogle.TextBase.All
import Hoogle.DataBase.Items
import Hoogle.DataBase.Suggest
import Hoogle.DataBase.TypeSearch
import Hoogle.DataBase.TextSearch

import General.Index


data DataBase = DataBase
    {items :: Items
    ,textSearch :: TextSearch
    ,typeSearch :: TypeSearch
    ,suggest :: Suggest
    }


createDataBase :: [TextItem] -> DataBase
createDataBase xs = DataBase items
        (createTextSearch ys) (createTypeSearch ys) (createSuggest ys)
    where (items,ys) = createItems xs


saveDataBase :: FilePath -> DataBase -> IO ()
saveDataBase = undefined


loadDataBase :: FilePath -> IO DataBase
loadDataBase = undefined


showDataBase :: DataBase -> String
showDataBase = undefined
