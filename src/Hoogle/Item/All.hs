
module Hoogle.Item.All where

import General.Index


data Package = Package
    {packageId :: Id
    ,packageName :: String
    ,packageVersion :: String
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
    ,entryModule :: Maybe (Lookup Module)
    ,entryText :: [EntryText]
    }


data EntryText = Keyword String
               | Text String
               | Focus String -- the bit text search starts from
               | ArgPos Int String
               | Result String
