#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.Program
import Distribution.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Exit

main = defaultMainWithHooks userHooks
    where
        userHooks = defaultUserHooks{
                            hookedPrograms = simpleProgram "trhsx" : hookedPrograms defaultUserHooks,
                            hookedPreProcessors = ("hsx",trhxs) : hookedPreProcessors defaultUserHooks
                        }
        
        trhxs :: BuildInfo -> LocalBuildInfo -> FilePath -> FilePath -> Int -> IO ExitCode
        trhxs build local inFile outFile verbose = do
                trhxs_exe <- lookupProgram "trhsx" (withPrograms local)
                case trhxs_exe of
                    Nothing -> error $ "trhsx not found, cannot process: " ++ inFile
                    Just prog -> rawSystemProgram verbose prog [inFile, outFile]
