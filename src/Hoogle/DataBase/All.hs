
module Hoogle.DataBase.All where


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
