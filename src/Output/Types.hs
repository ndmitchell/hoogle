{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, BangPatterns, GADTs #-}

module Output.Types(writeTypes, searchTypes, searchTypesDebug) where

{-
Approach:
Each signature is stored, along with a fingerprint
A quick search finds the most promising 100 fingerprints
A slow search ranks the 100 items, excluding some
-}

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Binary
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Generics.Uniplate.Data
import Data.Data
import System.FilePath
import System.IO.Extra
import Control.Monad.Extra
import Foreign.Storable
import Control.Applicative
import Numeric.Extra
import Prelude

import Input.Item
import General.Store
import General.IString
import General.Str
import General.Util


writeTypes :: StoreWrite -> Maybe FilePath -> [(Maybe TargetId, Item)] -> IO ()
writeTypes store debug xs = do
    let debugger ext body = whenJust debug $ \file -> writeFileUTF8 (file <.> ext) body
    inst <- return $ Map.fromListWith (+) [(fromIString x,1) | (_, IInstance (Sig _ [TCon x _])) <- xs]
    xs <- writeDuplicates store [(i, fromIString <$> t) | (Just i, ISignature t) <- xs]
    names <- writeNames store debugger inst xs
    xs <- return $ map (lookupNames names (error "Unknown name in writeTypes")) xs
    writeFingerprints store xs
    writeSignatures store xs


searchTypes :: StoreRead -> Sig String -> [TargetId]
searchTypes store q =
        concatMap (expandDuplicates $ readDuplicates store) $
        searchFingerprints store names 100 $
        lookupNames names name0 q
        -- map unknown fields to name0, i.e. _
    where
        names = readNames store


searchTypesDebug :: StoreRead -> (String, Sig String) -> [(String, Sig String)] -> [String]
searchTypesDebug store query answers = intercalate [""] $
    f False "Query" query : zipWith (\i -> f True ("Answer " ++ show i)) [1..] answers
    where
        qsig = lookupNames names name0 $ snd query
        names = readNames store

        f match name (raw, sig) =
            [name ++ ": " ++ raw
            ,"Sig String: " ++ prettySig sig
            ,"Sig Name: " ++ prettySig (fmap prettyName sn)
            ,"Fingerprint: " ++ prettyFingerprint fp] ++
            if not match then [] else
            ["Cost: " ++ maybe "X, no match" show (matchFingerprint qsig fp)
            ,"Explain: " ++ showExplain (matchFingerprintDebug qsig fp)]
            where
                sn = lookupNames names name0 sig
                fp = toFingerprint sn

                showExplain = intercalate ", " . map g . sortOn (either (const minBound) (negate . snd))
                g (Left s) = "X " ++ s
                g (Right (s, x)) = show x ++ " " ++ s


---------------------------------------------------------------------
-- NAME/CTOR INFORMATION

data TypesNames a where TypesNames :: TypesNames (BS.ByteString, V.Vector Name) deriving Typeable

-- Must be a unique Name per String.
-- First 0-99 are variables, rest are constructors.
-- More popular type constructors have higher numbers.
-- There are currently about 14K names, so about 25% of the bit patterns are taken
newtype Name = Name Word16 deriving (Eq,Ord,Show,Data,Typeable,Storable,Binary)

name0 = Name 0 -- use to represent _

isCon, isVar :: Name -> Bool
isVar (Name x) = x < 100
isCon = not . isVar

prettyName :: Name -> String
prettyName x@(Name i)
    | x == name0 = "_"
    | isVar x = "v" ++ show i
    | otherwise = "C" ++ show i


-- | Give a name a popularity, where 0 is least popular, 1 is most popular
popularityName :: Name -> Double
popularityName (Name n) | isVar $ Name n = error "Can't call popularityName on a Var"
                        | otherwise = fromIntegral (n - 100) / fromIntegral (maxBound - 100 :: Word16)

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


writeNames :: StoreWrite -> (String -> String -> IO ()) -> Map.Map String Int -> [Sig String] -> IO Names
writeNames store debug inst xs = do
    let sigNames (Sig ctx typ) = nubOrd ['~':x | Ctx x _ <- ctx] ++ nubOrd [x | TCon x _ <- universeBi typ]

    -- want to rank highly instances that have a lot of types, and a lot of definitions
    -- eg Eq is used and defined a lot. Constructor is used in 3 places but defined a lot.
    let freq :: Map.Map String Int = -- how many times each identifier occurs
            Map.unionWith (\typ sig -> sig + min sig typ) (Map.mapKeysMonotonic ('~':) inst) $
            Map.fromListWith (+) $ map (,1::Int) $ concatMap sigNames xs
    let names = spreadNames $ Map.toList freq
    debug "names" $ unlines [s ++ " = " ++ show n ++ " (" ++ show (freq Map.! s) ++ " uses)" | (s,n) <- names]
    names <- return $ sortOn fst names
    storeWrite store TypesNames (bstr0Join $ map fst names, V.fromList $ map snd names)
    let mp2 = Map.fromAscList names
    return $ Names $ \x -> Map.lookup x mp2


-- | Given a list of names, spread them out uniquely over the range [Name 100 .. Name maxBound]
--   Aim for something with a count of p to be at position (p / pmax) linear interp over the range
spreadNames :: [(a, Int)] -> [(a, Name)]
spreadNames [] = []
spreadNames (sortOn (negate . snd) -> xs@((_,limit):_)) = check $ f (99 + fromIntegral (length xs)) maxBound xs
    where
        check xs | all (isCon . snd) xs && length (nubOrd $ map snd xs) == length xs = xs
                 | otherwise = error "Invalid spreadNames"

        -- I can only assign values between mn and mx inclusive
        f :: Word16 -> Word16 -> [(a, Int)] -> [(a, Name)]
        f !mn !mx [] = []
        f mn mx ((a,i):xs) = (a, Name real) : f (mn-1) (real-1) xs
            where real = fromIntegral $ max mn $ min mx ideal
                  ideal = mn + floor (fromIntegral (min commonNameThreshold i) * fromIntegral (mx - mn) / fromIntegral (min commonNameThreshold limit))

-- WARNING: Magic constant.
-- Beyond this count names don't accumulate extra points for being common.
-- Ensures that things like Bool (4523 uses) ranks much higher than ShakeOptions (24 uses) by not having
-- [] (10237 uses) skew the curve too much and use up all the available bits of discrimination.
commonNameThreshold = 1024

readNames :: StoreRead -> Names
readNames store = Names $ \x -> Map.lookup (BS.pack x) mp
    where mp = Map.fromAscList $ zip (BS.split '\0' s) $ V.toList n
          (s, n) = storeRead store TypesNames


---------------------------------------------------------------------
-- DUPLICATION INFORMATION

data TypesDuplicates a where TypesDuplicates :: TypesDuplicates (Jagged TargetId) deriving Typeable

newtype Duplicates = Duplicates {expandDuplicates :: Int -> [TargetId]}

-- writeDuplicates xs == nub (map snd xs)
    -- all duplicates are removed, order of first element is preserved
-- (i,x) <- zip [0..] (writeDuplicates xs); expandDuplicates i == map fst (filter ((==) x . snd) xs)
    -- given the result at position i, expandDuplicates gives the TargetId's related to it

writeDuplicates :: Ord a => StoreWrite -> [(TargetId, Sig a)] -> IO [Sig a]
writeDuplicates store xs = do
    -- s=signature, t=targetid, p=popularity (incoing index), i=index (outgoing index)
    xs <- return $ map (second snd) $ sortOn (fst . snd) $ Map.toList $
        Map.fromListWith (\(x1,x2) (y1,y2) -> (, x2 ++ y2) $! min x1 y1)
                         [(s,(p,[t])) | (p,(t,s)) <- zip [0::Int ..] xs]
    -- give a list of TargetId's at each index
    storeWrite store TypesDuplicates $ jaggedFromList $ map (reverse . snd) xs
    return $ map fst xs

readDuplicates :: StoreRead -> Duplicates
readDuplicates store = Duplicates $ V.toList . ask
    where ask = jaggedAsk $ storeRead store TypesDuplicates


---------------------------------------------------------------------
-- FINGERPRINT INFORMATION

data TypesFingerprints a where TypesFingerprints :: TypesFingerprints (V.Vector Fingerprint) deriving Typeable

data Fingerprint = Fingerprint
    {fpRare1 :: {-# UNPACK #-} !Name -- Most rare ctor, or 0 if no rare stuff
    ,fpRare2 :: {-# UNPACK #-} !Name -- 2nd rare ctor
    ,fpRare3 :: {-# UNPACK #-} !Name -- 3rd rare ctor
    ,fpArity :: {-# UNPACK #-} !Word8 -- Artiy, where 0 = CAF
    ,fpTerms :: {-# UNPACK #-} !Word8 -- Number of terms (where 255 = 255 and above)
    } deriving (Eq,Show,Typeable)

prettyFingerprint :: Fingerprint -> String
prettyFingerprint Fingerprint{..} =
    "arity=" ++ show fpArity ++ ", terms=" ++ show fpTerms ++
    ", rarity=" ++ unwords (map prettyName [fpRare1, fpRare2, fpRare3])


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
          fpArity = fromIntegral $ min 255 $ max 0 $ pred $ length $ sigTy sig
          fpTerms = fromIntegral $ min 255 $ length (universeBi sig :: [Name])

writeFingerprints :: StoreWrite -> [Sig Name] -> IO ()
writeFingerprints store xs = storeWrite store TypesFingerprints $ V.fromList $ map toFingerprint xs

data MatchFingerprint a ma = MatchFingerprint
    {mfpAdd :: a -> a -> a
    ,mfpAddM :: ma -> ma -> ma
    ,mfpJust :: a -> ma
    ,mfpCost :: String -> Int -> a
    ,mfpMiss :: String -> ma
    }


matchFingerprint :: Sig Name -> Fingerprint -> Maybe Int
matchFingerprint = matchFingerprintEx MatchFingerprint{..}
    where
        mfpAdd = (+)
        mfpAddM = liftM2 (+)
        mfpJust = Just
        mfpCost _ x = x
        mfpMiss _ = Nothing

matchFingerprintDebug :: Sig Name -> Fingerprint -> [Either String (String, Int)]
matchFingerprintDebug = matchFingerprintEx MatchFingerprint{..}
    where
        mfpAdd = (++)
        mfpAddM = (++)
        mfpJust = id
        mfpCost s x = [Right (s,x)]
        mfpMiss s = [Left s]


{-# INLINE matchFingerprintEx #-}
matchFingerprintEx :: forall a ma . MatchFingerprint a ma -> Sig Name -> Fingerprint -> ma -- lower is better
matchFingerprintEx MatchFingerprint{..} sig@(toFingerprint -> target) =
    \candidate -> arity (fpArity candidate) `mfpAddM` terms (fpTerms candidate) `mfpAddM` rarity candidate
    where
        -- CAFs must match perfectly, otherwise too many is better than too few
        arity | ta == 0 = \ca -> if ca == 0 then mfpJust $ mfpCost "arity equal" 0 else mfpMiss "arity different and query a CAF" -- searching for a CAF
              | otherwise = \ca -> case fromIntegral $ ca - ta of
                    _ | ca == 0 -> mfpMiss "arity different and answer a CAF" -- searching for a CAF
                    0  -> mfpJust $ mfpCost "arity equal" 0 -- perfect match
                    -1 -> mfpJust $ mfpCost "arity 1 to remove" 1000 -- not using something the user carefully wrote
                    n | n > 0 && allowMore -> mfpJust $ mfpCost ("arity " ++ show n ++ " to add with wildcard") $ 300 * n -- user will have to make up a lot, but they said _ in their search
                    1  -> mfpJust $ mfpCost "arity 1 to add" 300  -- user will have to make up an extra param
                    2  -> mfpJust $ mfpCost "arity 2 to add"  900  -- user will have to make up two params
                    _ -> mfpMiss ""
            where
                ta = fpArity target
                allowMore = TVar name0 [] `elem` sigTy sig

        -- missing terms are a bit worse than invented terms, but it's fairly balanced, clip at large numbers
        terms = \ct -> case fromIntegral $ ct - tt of
                n | abs n > 20 -> mfpMiss $ "terms " ++ show n ++ " different" -- too different
                  | n == 0 -> mfpJust $ mfpCost "terms equal" 0
                  | n > 0 -> mfpJust $ mfpCost ("terms " ++ show n ++ " to add") $ n * 10 -- candidate has more terms
                  | otherwise -> mfpJust $ mfpCost ("terms " ++ show (-n) ++ " to remove") $ abs n * 12 -- candidate has less terms
            where
                tt = fpTerms target

        -- given two fingerprints, you have three sets:
        -- Those in common; those in one but not two; those in two but not one
        -- those that are different
        rarity = \cr -> let tr = target in mfpJust $
                differences 5000 400 tr cr `mfpAdd` -- searched for T but its not in the candidate, bad if rare, not great if common
                differences 1000  50 cr tr          -- T is in the candidate but I didn't search for it, bad if rare, OK if common
            where
                fpRaresElem :: Name -> Fingerprint -> Bool
                fpRaresElem !x = fpRaresFold (||) (== x)

                differences :: Double -> Double -> Fingerprint -> Fingerprint -> a
                differences !rare !common !want !have = fpRaresFold mfpAdd f want
                    where f n | fpRaresElem n have = mfpCost ("term in common " ++ prettyName n) 0
                              | n == name0 = mfpCost ("term _ missing") $ floor rare -- should this be common?
                              | otherwise = let p = popularityName n in mfpCost ("term " ++ prettyName n ++ " (" ++ showDP 2 p ++ ") missing") $
                                            floor $ (p*common) + ((1-p)*rare)


searchFingerprints :: StoreRead -> Names -> Int -> Sig Name -> [Int]
searchFingerprints store names n sig = map snd $ takeSortOn fst n [(v, i) | (i,f) <- zip [0..] fs, Just v <- [test f]]
    where fs = V.toList $ storeRead store TypesFingerprints :: [Fingerprint]
          test = matchFingerprint sig


---------------------------------------------------------------------
-- SIGNATURES

data TypesSigPositions a where TypesSigPositions :: TypesSigPositions (V.Vector Word32) deriving Typeable
data TypesSigData a where TypesSigData :: TypesSigData BS.ByteString deriving Typeable

writeSignatures :: StoreWrite -> [Sig Name] -> IO ()
writeSignatures store xs = do
    v <- VM.new $ length xs
    forM_ (zip [0..] xs) $ \(i,x) -> do
        let b = encodeBS x
        storeWritePart store TypesSigData b
        VM.write v i $ fromIntegral $ BS.length b
    v <- V.freeze v
    storeWrite store TypesSigPositions v
