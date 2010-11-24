
module Hoogle.DataBase.SubstrSearch
    (SubstrSearch, createSubstrSearch
    ,searchSubstrSearch
    ,completionsSubstrSearch
    ) where

import Data.Binary.Defer
import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import General.Code
import Hoogle.Type.All
import Hoogle.Score.All
import Data.Function
import Data.Array


{-
Description:

The substring search is an array of (key,value) pairs. The keys are sorted case
sensitively.

To do a search we binary chop to find the range where the exact prefixes lie,
then we binary chop to find the range where the inexact prefixes lie, then we
scan linearly to find all substrings.

Data is stored flattened. For default we expect ~200Kb of disk usage.
-}

-- keys are sorted after being made lower case
data SubstrSearch a = SubstrSearch (Array Int (BS.ByteString,a))


-- | Create a substring search index. Values are returned in order where possible.
createSubstrSearch :: [(String,a)] -> SubstrSearch a
createSubstrSearch xs = SubstrSearch $ listArray (0,length xs-1) $ map (first BS.pack) . sortBy (compare `on` fst) $ map (first $ map toLower) xs


searchSubstrSearch :: SubstrSearch a -> String -> [(a, EntryView, Score)]
searchSubstrSearch (SubstrSearch xs) y = map snd $
        find MatchExact (ly ==) $
        find MatchPrefix (ly `BS.isPrefixOf`) $
        find MatchSubstr (ly `BS.isInfixOf`) []
    where
        view = FocusOn y
        ly = BS.pack $ map toLower y

        xs2 = assocs xs
        find scr p rest = res ++ filter (flip notElem (map fst res) . fst) rest
            where res = [(i,(x,view,textScore scr)) | (i,(s,x)) <- xs2, p s]



completionsSubstrSearch :: SubstrSearch a -> String -> [String]
completionsSubstrSearch _ _ = []


instance Show a => Show (SubstrSearch a) where
    show x = "SubstrSearch"

instance (Bin.Binary a, BinaryDeferGet a) => BinaryDefer (SubstrSearch a) where
    put x = putDefer $ putLazyByteString $ Bin.runPut $ putBinary Bin.put x

    get = do
        g <- binaryDeferGet
        x <- getDefer getLazyByteString
        return $ Bin.runGet (getBinary g) x


putBinary :: (a -> Bin.Put) -> SubstrSearch a -> Bin.Put
putBinary p (SubstrSearch x) = do
    Bin.putWord32host $ fromIntegral $ snd (bounds x) + 1
    forM_ (elems x) $ \(s,a) -> do
        Bin.putWord8 $ fromIntegral $ BS.length s
        Bin.putByteString s
        p a


getBinary :: Bin.Get a -> Bin.Get (SubstrSearch a)
getBinary g = do
    i <- fmap fromIntegral Bin.getWord32host
    fmap (SubstrSearch . listArray (0,i-1)) $ replicateM i $ do
        n <- Bin.getWord8
        s <- Bin.getByteString (fromIntegral n)
        a <- g
        return (s,a)

