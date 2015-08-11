{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, BangPatterns #-}

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
import System.FilePath
import System.IO.Extra
import Control.Monad.Extra
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
    let debugger ext body = whenJust debug $ \file -> writeFileUTF8 (file <.> ext) body
    inst <- return $ Map.fromListWith (+) [(fromIString x,1) | (_, IInstance (Sig _ [TCon x _])) <- xs]
    xs <- writeDuplicates store [(i, fromIString <$> t) | (Just i, ISignature _ t) <- xs]
    names <- writeNames store (debugger "types.popularity") inst xs
    xs <- return $ map (lookupNames names (error "Unknown name in writeTypes")) xs
    writeFingerprints store $ map toFingerprint xs


searchTypes :: StoreRead -> Sig String -> [TargetId]
searchTypes store q =
        concatMap (expandDuplicates $ readDuplicates dupe1 dupe2) $
        searchFingerprints fingerprints names 100 $
        lookupNames names name0 q
        -- map unknown fields to name0, i.e. _
    where
        [dupe1, dupe2, readNames -> names, fingerprints] = storeReadList $ storeReadType Types store


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


-- | Giving "" to lookupName should always return the highest name
newtype Names = Names {lookupName :: String -> Maybe Name}

-- | Give a name a popularity, where 0 is least popular, 1 is most popular
popularityName :: Names -> Name -> Double
popularityName Names{..} = \(Name n) -> if isVar (Name n) then error "Can't call popularityName on a Var" else fromIntegral (n - 100) / fromIntegral (mx - 100)
    where Just (Name mx) = lookupName ""

lookupNames :: Names -> Name -> Sig String -> Sig Name
lookupNames Names{..} def (Sig ctx typ) = Sig (map f ctx) (map g typ)
    where
        vars = nubOrd $ "_" : [x | Ctx _ x <- ctx] ++ [x | TVar x _ <- universeBi typ]
        var x = Name $ min 99 $ fromIntegral $ fromMaybe (error "lookupNames") $ elemIndex x vars
        con = fromMaybe def . lookupName

        f (Ctx a b) = Ctx (con $ '~':a) (var b)
        g (TCon x xs) = TCon (con x) $ map g xs
        g (TVar x xs) = TVar (var x) $ map g xs


writeNames :: StoreWrite -> (String -> IO ()) -> Map.Map String Int -> [Sig String] -> IO Names
writeNames store debug inst xs = do
    let names (Sig ctx typ) = nubOrd ['~':x | Ctx x _ <- ctx] ++ nubOrd [x | TCon x _ <- universeBi typ]

    -- want to rank highly instances that have a lot of types, and a lot of definitions
    -- eg Eq is used and defined a lot. Constructor is used in 3 places but defined a lot.
    let mp = Map.unionWith (\typ sig -> sig + min sig typ) (Map.mapKeysMonotonic ('~':) inst) $
             Map.fromListWith (+) $ map (,1::Int) $ concatMap names xs
    let ns = sortOn snd $ Map.toList $ Map.delete "" mp
    debug $ unlines [n ++ " = " ++ show i ++ " (" ++ show c ++ " uses)" | ((n,c),i) <- zip ns $ map Name [100..]]
    ns <- return $ (++ [""]) $ map fst ns
    storeWriteBS store $ BS.pack $ intercalate "\0" ns
    let mp2 = Map.fromList $ zip ns $ map Name [100..]
    return $ Names $ \x -> Map.lookup x mp2

readNames :: StoreRead -> Names
readNames store = Names $ \x -> Map.lookup (BS.pack x) mp
    where mp = Map.fromList $ zip (BS.split '\0' $ storeReadBS store) $ map Name [100..]


---------------------------------------------------------------------
-- DUPLICATION INFORMATION

newtype Duplicates = Duplicates {expandDuplicates :: Int -> [TargetId]}

writeDuplicates :: Ord a => StoreWrite -> [(TargetId, Sig a)] -> IO [Sig a]
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

{-# INLINE fpRaresFold #-}
fpRaresFold :: (b -> b -> b) -> (Name -> b) -> Fingerprint -> b
fpRaresFold g f Fingerprint{..} = f fpRare1 `g` f fpRare2 `g` f fpRare3

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
    where fpRare1:fpRare2:fpRare3:_ = sort (nubOrd $ filter isCon $ universeBi sig) ++ [name0,name0,name0]
          fpArity = fromIntegral $ max 0 $ pred $ length $ sigTy sig
          fpTerms = fromIntegral $ min 255 $ length (universeBi sig :: [Name])

writeFingerprints :: StoreWrite -> [Fingerprint] -> IO ()
writeFingerprints store xs = storeWriteV store $ V.fromList xs

matchFingerprint :: (Name -> Double) -> Sig Name -> Fingerprint -> Maybe Int -- lower is better
matchFingerprint popularity sig@(toFingerprint -> target) = \candidate ->
    arity (fpArity candidate) +$+ terms (fpTerms candidate) +$+ rarity candidate
    where
        (+$+) = liftM2 (+)

        -- CAFs must match perfectly, otherwise too many is better than too few
        arity | ta == 0 = \ca -> if ca == 0 then Just 0 else Nothing -- searching for a CAF
              | otherwise = \ca -> case fromIntegral $ ca - ta of
                    _ | ca == 0 -> Nothing -- searching for a CAF
                    0  -> Just 0 -- perfect match
                    -1 -> Just 1000 -- not using something the user carefully wrote
                    n | n > 0 && allowMore -> Just $ 300 * n -- user will have to make up a lot, but they said _ in their search
                    1  -> Just 300  -- user will have to make up an extra param
                    2  -> Just 900  -- user will have to make up two params
                    _ -> Nothing
            where
                ta = fpArity target
                allowMore = TVar name0 [] `elem` sigTy sig

        -- missing terms are a bit worse than invented terms, but it's fairly balanced, clip at large numbers
        terms = \ct -> case fromIntegral $ ct - tt of
                n | abs n > 20 -> Nothing -- too different
                  | n > 0 -> Just $ n * 10 -- candidate has more terms
                  | otherwise -> Just $ n * 12 -- candidate has less terms
            where
                tt = fpTerms target

        rarity = \cr -> let tr = target in Just $ floor $
                differences 1000 400 tr cr + -- searched for T but its not in the candidate, bad if rare, not great if common
                differences 1000  50 cr tr   -- T is in the candidate but I didn't search for it, bad if rare, OK if common
            where
                fpRaresElem :: Name -> Fingerprint -> Bool
                fpRaresElem !x = fpRaresFold (||) (== x)

                differences :: Double -> Double -> Fingerprint -> Fingerprint -> Double
                differences !rare !common !want !have = fpRaresFold (+) f want
                    where f n | fpRaresElem n have = 0
                              | n == name0 = rare -- should this be common?
                              | otherwise = let p = popularity n in ((p*common) + ((1-p)*rare)) / 2


searchFingerprints :: StoreRead -> Names -> Int -> Sig Name -> [Int]
searchFingerprints store names n sig = map snd $ takeSortOn fst n [(v, i) | (i,f) <- zip [0..] fs, Just v <- [test f]]
    where fs = V.toList $ storeReadV store :: [Fingerprint]
          test = matchFingerprint (popularityName names) sig
