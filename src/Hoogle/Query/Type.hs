
module Hoogle.Query.Type where

import Data.Maybe
import Data.Char
import Data.Generics.UniplateOn

import General.Code
import Hoogle.TypeSig.All


usefulQuery query = not (null (names query)) || isJust (typeSig query)


blankQuery = Query [] [] Nothing []

data Query = Query {
        scope :: [Scope],
        names :: [String],
        typeSig :: Maybe TypeSig,
        flags :: [Flag]
    }
    deriving Show


instance Eq Query where
    (Query a1 b1 c1 d1) == (Query a2 b2 c2 d2) =
        and [a1 `setEq` a2, b1 `setEq` b2, c1 == c2, d1 `setEq` d2]


data Scope = PlusPackage  String
           | MinusPackage String
           | PlusModule  [String]
           | MinusModule [String]
           deriving (Eq, Show, Read)

isPlusModule  (PlusModule  _) = True; isPlusModule  _ = False
isMinusModule (MinusModule _) = True; isMinusModule _ = False


-- primarily for consoles, but some work on the web search
-- first is the name, second is optional data
data Flag = Flag String String
            deriving (Eq, Show, Read)


transformQueryType :: (Type -> Type) -> Query -> Query
transformQueryType f q = q{typeSig = maybe Nothing (Just . transformOn onTypeSig f) (typeSig q)}

universeQueryType :: Query -> [Type]
universeQueryType = maybe [] (universeOn onTypeSig) . typeSig
