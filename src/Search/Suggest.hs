
module Search.Suggest(
    createSuggest, mergeSuggest, searchSuggest
    ) where

import Search.Type
import Hoogle.Type.Item
import Hoogle.Type.TypeSig

createSuggest :: FilePath -> Package -> [Fact] -> IO ()
createSuggest = undefined


mergeSuggest :: FilePath -> [Package] -> Package -> IO ()
mergeSuggest = undefined


searchSuggest :: FilePath -> [Package] -> (Maybe String, Maybe TypeSig) -> IO (Maybe (Maybe String, Maybe TypeSig))
searchSuggest = undefined
