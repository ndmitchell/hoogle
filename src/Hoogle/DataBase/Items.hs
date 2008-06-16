
module Hoogle.DataBase.Items where

import General.Index


data Items = Items
    {packages :: Index Package
    ,modules :: Index Module
    ,entrys :: Index Entry
    }


data Package = Package
    {packageId :: Id
    ,packageName :: String
    ,haddockURL :: String
    ,hscolourURL :: String
    }


data Module = Module
    {moduleId :: Id
    ,moduleName :: [String]
    ,modulePackage :: Lookup Package
    }


data Entry = Entry
    {entryId :: Id
    ,entryModule :: Lookup Module
    ,entryText :: String
    }
