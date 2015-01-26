
import Data.List.Extra
import Control.Monad
import System.Directory.Extra
import Data.Time.Clock
import System.Process.Extra
import Data.Time.Clock
import Data.Time.Format
import System.FilePath
import System.Locale


-- APPROACH
-- have a directory "hogle-upgrade" in which you do your work
-- inside each directory is a datetime name, e.g. hogle-upgrade/20150126T001005
-- first delete all directories that are not the most recent 10
-- then create the new directory, git pull, cabal update, build, generate packages, test
-- kill the old server, start the new one
-- add yourself to a list of who was successfully started
-- write out a file in your directory called downgrade.sh, to do a quick downgrade if necessary
main :: IO ()
main = do
    createDirectoryIfMissing True "hogle-upgrade"
    dirs <- filterM doesDirectoryExist =<< listContents "hogle-upgrade"
    forM_ (dropEnd 10 $ sort dirs) $ \dir -> do
        putStrLn $ "Cleaning " ++ dir ++ "..."
        removeDirectoryRecursive dir

    now <- getCurrentTime
    let dir = "hogle-upgrade/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now
    now <- createDirectoryIfMissing True dir
    withCurrentDirectory dir $ do
        putStrLn $ "Upgrading into " ++ dir
        system_ "git clone https://github.com/ndmitchell/hogle.git ."
        system_ "cabal update"
        system_ "cabal configure \"--ghc-options=-rtsopts -O2\""
        system_ "cabal build"
        let exe = normalise "dist/build/hogle/hogle"
        system_ $ exe ++ " generate +RTS -M1G"
        system_ $ exe ++ " test"
        system_ "pkill hogle"
        system_ $ "nohup " ++ exe ++ " server --port=8080 --log=../../log.txt >> ../../out.txt &"
        writeFile "downgrade.sh" "pkill hogle\nnohup dist/build/hogle/hogle server --port=8080 --log=../../log.txt &\n"
    appendFile "hogle-upgrade/upgrade.txt" $ dir ++ "\n"
    putStrLn "Successfully upgraded"

