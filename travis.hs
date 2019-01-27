
import System.Time.Extra
import System.Process.Extra
import Control.Exception.Extra


main :: IO ()
main = do
    let timed x = do
        putStrLn $ "\n\nSearching for " ++ x
        (time,_) <- duration $ system_ $ "hoogle +RTS -T -M1.8G -K2000K -RTS " ++ show x
        putStrLn $ "Search " ++ show x ++ " took " ++ showDuration time
        putStrLn "\n\n"
    retry 3 $ timed "generate"
    timed "test"
    timed "map"
    timed "map package:base"
    timed ":: a -> b"
    timed ":: ShakeOptions -> a"
    retry 3 $ timed "generate --frege"
