{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Plugin.HolePlugin (Plugin.HolePlugin.plugin) where

import Action.Search (withSearch)
import Data.Aeson
import Data.Binary (Word32)
import Data.List
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
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
    split,
    text,
  )
import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Types.Constraint (Hole (hole_occ, hole_ty), HoleSort (TypeHole), ctPred, hole_ty)
import GHC.Tc.Utils.Monad
import Hoogle (defaultDatabaseLocation, searchDatabase', searchTargets)
import Input.Item (Target (targetItem), TargetId (TargetId), unHTMLTarget)
import Network.HTTP.Conduit (simpleHttp)
import Output.Items (lookupItem)
import System.IO
import Text.Read

newtype HolePluginState = HPS {host :: String}

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
initPlugin [host] = do
  newTcRef HPS {host = host}
initPlugin _ = error "Please specify the ranking host."

searchTargetIdsOnly :: String -> IO [TargetId]
searchTargetIdsOnly q = do
  database <- defaultDatabaseLocation
  res <- withSearch database $ \store -> do
    return $ searchDatabase' store q
  return $ map fst res

lookupTargets :: [TargetId] -> IO [Target]
lookupTargets ids = do
  database <- defaultDatabaseLocation
  withSearch database $ \store -> do
    let result = map (lookupItem store) ids
    return result

rankExternally :: String -> String -> [TargetId] -> IO [TargetId]
rankExternally host q ids = do
  let sepIds = intercalate "," (map (\(TargetId id) -> show id) ids)
  request <- simpleHttp $ host ++ "/rank?query=" ++ q ++ "&hoogle_ids=" ++ sepIds
  let res = map TargetId <$> (decode request :: Maybe [Word32])
  return $ fromMaybe [] res

searchHoogle :: String -> String -> String -> IO [String]
searchHoogle host q ts = do
  result <- searchTargetIdsOnly ts
  ranked <- if not $ null q then rankExternally host q result else return result
  map (targetItem . unHTMLTarget) <$> lookupTargets ranked

holeNameToQuery :: TypedHole -> String
holeNameToQuery hole = splitUcWords holeName
  where
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
  HPS {..} <- readTcRef ref
  res <- case holeT of
    Nothing -> return []
    Just ty -> liftIO $ do
      let holeQ = holeNameToQuery hole
      let searchP = ":: " ++ ty
      print $ "Host: " ++ host
      print $ "Type: " ++ searchP
      print $ "Query: " ++ holeQ
      searchHoogle host holeQ searchP
  return $ (take 10 $ map (RawHoleFit . text . ("Hoogle: " ++)) res) ++ hfs

plugin :: Plugin
plugin = defaultPlugin {holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where
    initP = initPlugin opts
    stopP = const $ return ()
    pluginDef ref = HoleFitPlugin {fitPlugin = hoogleRerankPlugin opts ref, candPlugin = defaultCandPlugin opts ref}
