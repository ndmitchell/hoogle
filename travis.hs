
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    let timed x = do
        putStrLn $ "\n\nSearching for " ++ x
        (time,_) <- duration $ system $ "hogle " ++ (if x == "" then "" else show x)
        putStrLn $ "Search " ++ show x ++ " took " ++ showDuration time
        putStrLn "\n\n"
    timed ""
    timed "map"
    timed "map package:base"
    timed ":: a -> b"
    timed ":: ShakeOptions -> a"
