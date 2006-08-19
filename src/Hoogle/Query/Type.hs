
module Hoogle.Query.Type where

import Hoogle.General
import Hoogle.TypeSig.All


data Query = Query {
        scope :: [Scope],
        names :: [String],
        typeSig :: Maybe TypeSig,
        items :: [ItemType],
        flags :: [Flag]
    }
    deriving (Show, Read)


instance Eq Query where
    (Query a1 b1 c1 d1 e1) == (Query a2 b2 c2 d2 e2) =
        (a1 `setEq` a2) && (b1 `setEq` b2) && (c1 == c2) && (d1 `setEq` d2) && (e1 `setEq` e2)


data Scope = PlusPackage  String
           | MinusPackage String
           | PlusModule  [String]
           | MinusModule [String]
           deriving (Eq, Show, Read)


data ItemType = ItemModule
              | ItemType
              | ItemFunction
              | ItemClass
              deriving (Eq, Show, Read)

itemTypes =
    [(["module"], ItemModule)
    ,(["type","data"], ItemType)
    ,(["function","ctor","fun"], ItemFunction)
    ,(["class","instance"], ItemClass)
    ]


-- primarily for consoles, but some work on the web search
data Flag = Info
          | Help
          | Version
          | Color
          | Count Int
          | Verbose
          | Path String
          deriving (Eq, Show, Read)


data FlagType = FlagNull Flag
              | FlagInt (Int -> Flag)
              | FlagStr (String -> Flag)


data FlagInfo = FlagInfo {flagType :: FlagType, flagChar :: [Char], flagStr :: String, flagDesc :: String}


flagInfos =
    [FlagInfo (FlagNull Info) "i" "info" "..."
    ,FlagInfo (FlagNull Help) "h?" "help" "..."
    ,FlagInfo (FlagNull Version) "" "version" "..."
    ,FlagInfo (FlagNull Color) "c" "color" "..."
    ,FlagInfo (FlagInt Count) "n" "count" "..."
    ,FlagInfo (FlagNull Verbose) "v" "verbose" "..."
    ,FlagInfo (FlagStr Path) "l" "path" "..."
    ]

