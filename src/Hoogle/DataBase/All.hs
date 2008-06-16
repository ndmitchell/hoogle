
module Hoogle.DataBase.All where


import Hoogle.DataBase.Item
import Hoogle.DataBase.Suggest
import Hoogle.DataBase.TypeSearch
import Hoogle.DataBase.TextSearch

import General.Index


data DataBase = DataBase
    {packages :: Index Package
    ,modules :: Index Module
    ,items :: Index Item
    ,textSearch :: TextSearch
    ,typeSearch :: TypeSearch
    ,suggest :: Suggest
    }
