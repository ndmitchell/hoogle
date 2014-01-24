{-# LANGUAGE RecordWildCards #-}

module Hoogle.DataBase2.Str(
    createStr', searchStr',
    createStr, mergeStr, searchStr
    ) where

import General.Base
import Hoogle.DataBase2.Type
import Hoogle.Type.All
import Hoogle.Score.All
import General.Util
import Data.Binary
import System.IO.Unsafe
import System.FilePath
import qualified General.FMIndex as FM
import qualified Data.ByteString.Char8 as BS


data Strs = Strs
    {posMaximum :: Pos
    ,posOffset :: [(Package, Pos)]
    ,fmIndex :: FM.FMIndex Pos
    } deriving Show

posResolve :: Strs -> Pos -> (Package, Pos)
posResolve Strs{..} p = f posOffset
    where
        f [(pkg,off)] = (pkg,p-off)
        f ((p1,o1):(p2,o2):rest)
            | p < o2 = (p1,p-o1)
            | otherwise = f $ (p2,o2):rest


instance Binary Strs where
    put (Strs a b c) = put a >> put b >> put c
    get = Strs <$> get <*> get <*> get


saveStr :: FilePath -> Strs -> IO ()
saveStr = encodeFile

loadStr :: FilePath -> IO Strs
loadStr = decodeFile


createStr :: Package -> [(Pos, BS.ByteString)] -> FilePath -> IO ()
createStr pkg items file = saveStr file $ Strs (maximum $ 0 : map fst items) [(pkg, 0)] $ FM.create '\0' $ map ((BS.map toLower . snd) &&& fst) items

mergeStr :: [FilePath] -> FilePath -> IO ()
mergeStr xs file = do
    let f mx Strs{..} = (mx + posMaximum, Strs 0 (map (second (+mx)) posOffset) (fmap (+mx) fmIndex))
    (mx,xs) <- mapAccumL f 0 <$> mapM loadStr xs
    saveStr file $ Strs mx (concatMap posOffset xs) (FM.create '\0' $ concatMap (FM.extract . fmIndex) xs)


searchStr :: [FilePath] -> BS.ByteString -> IO [(Package, Pos, [EntryView], Score)]
searchStr files x = do
    files <- mapM loadStr files
    let locate (how1,how2) =
            [ ((pkg,pos),(pkg,pos,[FocusOn $ BS.unpack x],textScore how2))
            | file <- files
            , ((pkg,pos),_) <- map (first $ posResolve file) $ FM.locate (fmIndex file) how1 $ BS.map toLower x]
    return $ map snd $ nubOrdOn fst $ concatMap locate [(FM.Exact,MatchExact), (FM.Prefix,MatchPrefix), (FM.Infix,MatchSubstr)]


---------------------------------------------------------------------

createStr' :: Package -> [(Pos, Entry)] -> FilePath -> IO ()
createStr' pkg items out = createStr pkg (mapMaybe f items) out
    where f (pos, Entry{..}) = if null entryKey then Nothing else Just (pos, BS.pack entryKey)

searchStr' :: (String -> Word32 -> IO Entry) -> [FilePath] -> String -> IO [Result]
searchStr' resolve files x = do
    res <- searchStr (map (<.> "str") files) $ BS.pack x
    return $ flip map res $ \(Package a,Pos b,c,d) -> Result (unsafePerformIO $ resolve (BS.unpack a) b) c d
