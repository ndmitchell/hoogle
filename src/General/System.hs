{-# LANGUAGE CPP #-}

-- | Module for system like things in base/directory/etc, or could plausibly be added.
module General.System(module General.System, module X) where

import System.Cmd as X
import System.Directory as X
import System.Environment as X
import System.Exit as X
import System.IO as X
import System.Mem as X (performGC)

#ifndef mingw32_HOST_OS
import System.Posix(setFileCreationMask)
#endif


import General.Base
import qualified Control.Exception as E

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
#endif


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)


withModeGlobalRead :: IO () -> IO ()
#ifdef mingw32_HOST_OS
withModeGlobalRead act = act
#else
withModeGlobalRead act = E.bracket
    (setFileCreationMask 0o022)
    (const act)
    setFileCreationMask
#endif

-- FIXME: This could use a lot more bracket calls!
captureOutput :: IO () -> IO (Maybe String)
#if __GLASGOW_HASKELL__ < 612
captureOutput act = return Nothing
#else
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
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
