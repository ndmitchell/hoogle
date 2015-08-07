{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Output.Types(writeTypes, searchTypes) where

{-
Approach:
Each signature is stored, along with a fingerprint
A quick search finds the most promising 100 fingerprints
A slow search ranks the 100 items, excluding some
-}

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import Data.Maybe
import Data.Word
import Data.List.Extra
import Data.Tuple.Extra
import Data.Generics.Uniplate.Data
import Data.Data
import Foreign.Storable
import Control.Applicative
import Prelude

import Input.Item
import General.Store
import General.IString
import General.Util


data Types = Types deriving Typeable

writeTypes :: StoreWrite -> Maybe FilePath -> [(Maybe TargetId, Item)] -> IO ()
writeTypes store debug xs = storeWriteType store Types $ do
    xs <- return [(i, fromIString <$> t) | (Just i, ISignature _ t) <- xs]
    names <- writeNames store $ map snd xs
    xs <- writeDuplicates store $ map (second $ lookupNames names (error "Unknown name in writeTypes")) xs
    writeFingerprints store $ map toFingerprint xs


searchTypes :: StoreRead -> Sig String -> [TargetId]
searchTypes store q =
        concatMap (expandDuplicates $ readDuplicates dupe1 dupe2) $
        searchFingerprints fingerprints 100 $
        lookupNames (readNames names) name0 q
        -- map unknown fields to name0, i.e. _
    where
        [names, dupe1, dupe2, fingerprints] = storeReadList $ storeReadType Types store


---------------------------------------------------------------------
-- NAME/CTOR INFORMATION

-- Must be a unique Name per String.
-- First 0-99 are variables, rest are constructors.
-- More popular type constructors have higher numbers.
newtype Name = Name Word16 deriving (Eq,Ord,Show,Data,Typeable,Storable)

name0 = Name 0 -- use to represent _

isCon, isVar :: Name -> Bool
isVar (Name x) = x < 100
isCon = not . isVar


newtype Names = Names {lookupName :: String -> Maybe Name}

lookupNames :: Names -> Name -> Sig String -> Sig Name
lookupNames Names{..} def (Sig ctx typ) = Sig (map f ctx) (map g typ)
    where
        vars = nubOrd $ "_" : [x | Ctx _ x <- ctx] ++ [x | TVar x _ <- universeBi typ]
        var x = Name $ min 99 $ fromIntegral $ fromMaybe (error "lookupNames") $ elemIndex x vars
        con = fromMaybe def . lookupName

        f (Ctx a b) = Ctx (con $ '~':a) (var b)
        g (TCon x xs) = TCon (con x) $ map g xs
        g (TVar x xs) = TVar (var x) $ map g xs


writeNames :: StoreWrite -> [Sig String] -> IO Names
writeNames store xs = do
    let names (Sig ctx typ) = nubOrd ['~':x | Ctx x _ <- ctx] ++ nubOrd [x | TCon x _ <- universeBi typ]
    let mp = Map.fromListWith (+) $ map (,1::Int) $ concatMap names xs
    let ns = map fst $ sortOn snd $ Map.toList mp
    storeWriteBS store $ BS.pack $ intercalate "\0" ns
    let mp2 = Map.fromList $ zip ns $ map Name [100..]
    return $ Names $ \x -> Map.lookup x mp2

readNames :: StoreRead -> Names
readNames store = Names $ \x -> Map.lookup (BS.pack x) mp
    where mp = Map.fromList $ zip (BS.split '\0' $ storeReadBS store) $ map Name [100..]


---------------------------------------------------------------------
-- DUPLICATION INFORMATION

newtype Duplicates = Duplicates {expandDuplicates :: Int -> [TargetId]}

writeDuplicates :: StoreWrite -> [(TargetId, Sig Name)] -> IO [Sig Name]
writeDuplicates store xs = do
    -- s=signature, t=targetid, p=popularity (incoing index), i=index (outgoing index)
    xs <- return $ map (second snd) $ sortOn (fst . snd) $ Map.toList $
        Map.fromListWith (\(x1,x2) (y1,y2) -> (min x1 y1, x2 ++ y2)) [(s,(p,[t])) | (p,(t,s)) <- zip [0::Int ..] xs]
    let (is,ts) = unzip [(i::Word32, t) | (i,(_,ts)) <- zip [0..] xs, t <- reverse ts]
    storeWriteV store $ V.fromList is
    storeWriteV store $ V.fromList ts
    return $ map fst xs

readDuplicates :: StoreRead -> StoreRead -> Duplicates
readDuplicates (storeReadV -> is) (storeReadV -> ts) = Duplicates $ \i -> map snd $ filter ((==) (fromIntegral i) . fst) xs
    where xs = zip (V.toList is :: [Word32]) (V.toList ts :: [TargetId])


---------------------------------------------------------------------
-- FINGERPRINT INFORMATION

data Fingerprint = Fingerprint
    {fpRare1 :: {-# UNPACK #-} !Name -- Most rare ctor, or 0 if no rare stuff
    ,fpRare2 :: {-# UNPACK #-} !Name -- 2nd rare ctor
    ,fpRare3 :: {-# UNPACK #-} !Name -- 3rd rare ctor
    ,fpArity :: {-# UNPACK #-} !Word8 -- Artiy, where 0 = CAF
    ,fpTerms :: {-# UNPACK #-} !Word8 -- Number of terms (where 255 = 255 and above)
    } deriving (Eq,Show)

instance Storable Fingerprint where
    sizeOf _ = 64
    alignment _ = 4
    peekByteOff ptr i = Fingerprint
        <$> peekByteOff ptr (i+0) <*> peekByteOff ptr (i+2) <*> peekByteOff ptr (i+4)
        <*> peekByteOff ptr (i+6) <*> peekByteOff ptr (i+7)
    pokeByteOff ptr i Fingerprint{..} = do
        pokeByteOff ptr (i+0) fpRare1 >> pokeByteOff ptr (i+2) fpRare2 >> pokeByteOff ptr (i+4) fpRare3
        pokeByteOff ptr (i+6) fpArity >> pokeByteOff ptr (i+7) fpTerms

toFingerprint :: Sig Name -> Fingerprint
toFingerprint sig = Fingerprint{..}
    where fpRare1:fpRare2:fpRare3:_ = reverse (sort $ nubOrd $ filter isCon $ universeBi sig) ++ [name0,name0,name0]
          fpArity = fromIntegral $ length $ sigTy sig
          fpTerms = fromIntegral $ min 255 $ length (universeBi sig :: [Name])

writeFingerprints :: StoreWrite -> [Fingerprint] -> IO ()
writeFingerprints store xs = storeWriteV store $ V.fromList xs

matchFingerprint :: Sig Name -> Fingerprint -> Maybe Int -- lower is better
matchFingerprint sig = \x -> if x == fp then Just 0 else Nothing
    where fp = toFingerprint sig

searchFingerprints :: StoreRead -> Int -> Sig Name -> [Int]
searchFingerprints store n sig = map snd $ takeSortOn fst n [(v, i) | (i,f) <- zip [0..] fs, Just v <- [test f]]
    where fs = V.toList $ storeReadV store :: [Fingerprint]
          test = matchFingerprint sig
