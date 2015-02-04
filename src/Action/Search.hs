{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Search(actionSearch, search) where

import Control.Applicative
import System.FilePath
import Control.Monad.Extra
import qualified Data.Set as Set
import Data.List.Extra

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import General.Store
import Query
import Input.Type
import Action.CmdLine


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

actionSearch :: CmdLine -> IO ()
actionSearch Search{..} = do
    let pkg = [database | database /= ""]
    let rest = query
    forM_ (if null pkg then ["all"] else pkg) $ \pkg ->
        readStoreFile ("output" </> pkg <.> "hoo") $ \store -> do
            res <- search store $ parseQuery $ unwords rest
            forM_ (maybe id take count res) $ putStrLn . prettyItem . itemItem

search :: StoreIn -> Query -> IO [ItemEx]
search store (Query strs typ qtags) = do
    let tags = readTags store
    let exact = Scope True "is" "exact" `elem` qtags
    is <- case (strs, typ) of
        ([], Nothing) | not $ null qtags, xs@(_:_) <- searchTags tags qtags -> return xs
                      | otherwise -> return $ searchNames store exact []
        ([], Just t ) -> searchTypes store t
        (xs, Nothing) -> return $ searchNames store exact xs
        (xs, Just t ) -> do
            nam <- return $ Set.fromList $ searchNames store exact xs
            filter (`Set.member` nam) <$> searchTypes store t
    let look = lookupItem store
    return $ map (look . snd) $ sortOn fst $ filter (filterTags tags qtags . snd) is
