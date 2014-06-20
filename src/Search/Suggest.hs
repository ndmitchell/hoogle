
module Search.Suggest(
    createSuggest, mergeSuggest, searchSuggest
    ) where

import Search.Type

createSuggest :: FilePath -> Package -> [Stm] -> IO ()
createSuggest = undefined


mergeSuggest :: FilePath -> [Package] -> Package -> IO ()
mergeSuggest = undefined


searchSuggest :: FilePath -> [Package] -> (Maybe String, Maybe Typ) -> IO (Maybe (Maybe String, Maybe Typ))
searchSuggest = undefined
