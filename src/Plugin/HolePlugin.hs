{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Plugin.HolePlugin (Plugin.HolePlugin.plugin) where

import Data.List
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import GHC.Plugins
  ( CandPlugin,
    CommandLineOption,
    FitPlugin,
    GlobalRdrElt (gre_imp),
    HasDynFlags (getDynFlags),
    HoleFit (HoleFit, RawHoleFit, hfCand),
    HoleFitCandidate (GreHFCand),
    HoleFitPlugin (HoleFitPlugin, candPlugin, fitPlugin),
    HoleFitPluginR (HoleFitPluginR),
    Outputable (ppr),
    Plugin (holeFitPlugin, pluginRecompile),
    TypedHole (th_hole),
    defaultPlugin,
    hang,
    importSpecModule,
    liftIO,
    moduleNameString,
    occNameString,
    purePlugin,
    showSDoc,
    text,
  )
import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Types.Constraint (Hole (hole_occ, hole_ty), HoleSort (TypeHole), ctPred, hole_ty)
import GHC.Tc.Utils.Monad
import Hoogle (searchTargets)
import Input.Item (Target (targetItem), unHTMLTarget)
import System.IO
import Text.Read

data HolePluginState = HPS
  { timeAlloted :: Maybe NominalDiffTime,
    elapsedTime :: NominalDiffTime,
    timeCurStarted :: UTCTime
  }

bumpElapsed :: NominalDiffTime -> HolePluginState -> HolePluginState
bumpElapsed ad (HPS a e t) = HPS a (e + ad) t

setAlloted :: Maybe NominalDiffTime -> HolePluginState -> HolePluginState
setAlloted a (HPS _ e t) = HPS a e t

setCurStarted :: UTCTime -> HolePluginState -> HolePluginState
setCurStarted nt (HPS a e _) = HPS a e nt

hpStartState :: HolePluginState
hpStartState = HPS Nothing zero undefined
  where
    zero = fromInteger @NominalDiffTime 0

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
initPlugin [msecs] = newTcRef $ hpStartState {timeAlloted = alloted}
  where
    errMsg = "Invalid amount of milliseconds given to plugin: " <> show msecs
    alloted = case readMaybe @Integer msecs of
      Just millisecs -> Just $ fromInteger @NominalDiffTime millisecs / 1000
      _ -> error errMsg
initPlugin _ = newTcRef hpStartState

fromModule :: HoleFitCandidate -> [String]
fromModule (GreHFCand gre) =
  map (moduleNameString . importSpecModule) $ gre_imp gre
fromModule _ = []

toHoleFitCommand :: TypedHole -> String -> Maybe String
toHoleFitCommand tHole str =
  case th_hole tHole of
    Just hole -> stripPrefix ("_" <> str) $ occNameString $ hole_occ hole
    _ -> Nothing

-- | This candidate plugin filters the candidates by module,
-- using the name of the hole as module to search in
modFilterTimeoutP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
modFilterTimeoutP _ ref hole cands = do
  curTime <- liftIO Time.getCurrentTime
  HPS {..} <- readTcRef ref
  updTcRef ref (setCurStarted curTime)
  return $ case timeAlloted of
    -- If we're out of time we remove all the candidates. Then nothing is checked.
    Just sofar | elapsedTime > sofar -> []
    _ -> case toHoleFitCommand hole "only_" of
      Just modName -> filter (inScopeVia modName) cands
      _ -> cands
  where
    inScopeVia modNameStr cand@(GreHFCand _) =
      elem (toModName modNameStr) $ fromModule cand
    inScopeVia _ _ = False
    toModName = replace '_' '.'
    replace :: Eq a => a -> a -> [a] -> [a]
    replace _ _ [] = []
    replace a b (x : xs) = (if x == a then b else x) : replace a b xs

searchHoogle :: String -> IO [String]
searchHoogle ts = do
  result <- map unHTMLTarget <$> searchTargets ts
  return $ map targetItem result

modSortP :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
modSortP _ ref hole hfs = do
  dflags <- getDynFlags
  let holeT = (showSDoc dflags . ppr) . hole_ty <$> th_hole hole
  res <- case holeT of
    Nothing -> return []
    Just ty -> liftIO $ searchHoogle ty
  return $ (take 10 $ map (RawHoleFit . text . ("Hoogle: " ++)) res) ++ hfs

plugin :: Plugin
plugin = defaultPlugin {holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where
    initP = initPlugin opts
    stopP = const $ return ()
    pluginDef ref =
      HoleFitPlugin
        { candPlugin = modFilterTimeoutP opts ref,
          fitPlugin = modSortP opts ref
        }
