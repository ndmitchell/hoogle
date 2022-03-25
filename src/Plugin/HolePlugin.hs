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
    text, split,
  )
import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Types.Constraint (Hole (hole_occ, hole_ty), HoleSort (TypeHole), ctPred, hole_ty)
import GHC.Tc.Utils.Monad
import Hoogle (searchTargets, searchTargetIdsOnly, rankExternally, lookupTargets)
import Input.Item (Target (targetItem), unHTMLTarget)
import System.IO
import Text.Read
import Data.Maybe (fromMaybe)
import Data.List.Extra (splitOn)

data HolePluginState = HPS

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
initPlugin [msecs] = newTcRef HPS  where
    errMsg = "Invalid amount of milliseconds given to plugin: " <> show msecs
    alloted = case readMaybe @Integer msecs of
      Just millisecs -> Just $ fromInteger @NominalDiffTime millisecs / 1000
      _ -> error errMsg
initPlugin _ = newTcRef HPS

searchHoogle :: String -> String -> IO [String]
searchHoogle q ts = do
  result <- searchTargetIdsOnly ts
  ranked <- if not $ null q then rankExternally q result else return result
  map (targetItem . unHTMLTarget) <$> lookupTargets ranked

holeNameToQuery :: TypedHole -> String
holeNameToQuery hole = splitUcWords holeName where
  holeName = case th_hole hole of
    Nothing -> ""
    Just ho -> fromMaybe "" . stripPrefix "_" . occNameString $ hole_occ ho
  splitUcWords = unwords . splitOn "_"

defaultCandPlugin :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
defaultCandPlugin _ _ _ cands = return cands

hoogleRerankPlugin :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
hoogleRerankPlugin _ ref hole hfs = do
  dflags <- getDynFlags
  let holeT = (showSDoc dflags . ppr) . hole_ty <$> th_hole hole
  res <- case holeT of
    Nothing -> return []
    Just ty -> liftIO $ do
      let holeQ = holeNameToQuery hole
      let searchP = ":: " ++ ty
      print $ "Type: " ++ searchP
      print $ "Query: " ++ holeQ
      searchHoogle holeQ searchP
  return $ (take 10 $ map (RawHoleFit . text . ("Hoogle: " ++)) res) ++ hfs

plugin :: Plugin
plugin = defaultPlugin {holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where
    initP = initPlugin opts
    stopP = const $ return ()
    pluginDef ref = HoleFitPlugin { fitPlugin = hoogleRerankPlugin opts ref, candPlugin = defaultCandPlugin opts ref}

