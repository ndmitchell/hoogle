
module Hoogle.DataBase.Item where

import General.Index


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


data Item = Item
    {itemId :: Id
    ,itemModule :: Lookup Module
    ,itemText :: String
    }
