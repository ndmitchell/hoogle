{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | Module for system like things in base/directory/etc, or could plausibly be added.
module General.System(module General.System, module X) where

import System.Process as X
import System.Directory as X
import System.Environment as X
import System.Exit as X
import System.IO as X
import System.Mem as X (performGC)

import General.Base
import qualified Control.Exception as E

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
#endif

#ifndef mingw32_HOST_OS
import System.Posix(setFileCreationMask)
#else
setFileCreationMask :: Int -> IO Int
setFileCreationMask _ = return 0
#endif


isWindows :: Bool
#ifdef mingw32_HOST_OS
isWindows = True
#else
isWindows = False
#endif


removeFile_ x = removeFile x `E.catch` \(_ :: E.SomeException) -> return ()


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)


withModeGlobalRead :: IO () -> IO ()
withModeGlobalRead act = E.bracket
    (setFileCreationMask 0o022)
    (\x -> setFileCreationMask x >> return ())
    (const act)


-- FIXME: This could use a lot more bracket calls!
captureOutput :: IO () -> IO (Maybe String)
#if __GLASGOW_HASKELL__ < 612
captureOutput act = return Nothing
#else
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hoogle"
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    res <- readFile' f
    removeFile f
    return $ Just res
#endif


system_ :: String -> IO ()
system_ x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "System command failed: " ++ x


exitMessage :: [String] -> IO a
exitMessage msg = putStr (unlines msg) >> exitFailure


getEnvVar :: String -> IO (Maybe String)
getEnvVar x = E.catch (fmap Just $ getEnv x) (\(x :: E.SomeException) -> return Nothing)
