
-- Draft of Hoogle API, does not yet compile!
module Hoogle(
    Query(..), parseQuery,
    ItemEx(..), Item(..), showItem,
    withSearch
    ) where

import qualified Query as Q
import qualified Input.Type as I
import qualified General.Store as S
import qualified Action.Search as S


newtype Database = Database StoreRead

withDatabase :: FilePath -> (Database -> IO ()) -> IO ()
withDatabase file act = withStoreFile file $ act . Database

search :: Database -> [Query] -> [ItemEx]
search = undefined
