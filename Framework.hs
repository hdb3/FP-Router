
import System.Environment(getArgs)
import System.IO
import System.Exit
import qualified Data.Map as M
import Control.Concurrent
import Message
import Config

framework routers links = do
    putStrLn "Hello from Framework"
    putStrLn $ "I have " ++ show (length routers) ++ " routers and " ++ show (M.size links) ++ " links."


main = do
    args <- getArgs
    let arg1 = if null args then "config.txt" else args !! 0
    putStrLn $ "Using file " ++ arg1 ++ " for configuration"
    (routers,links) <- fileConfig arg1
    frameworkThread <- forkIO (framework routers links)
    hSetBuffering stdin NoBuffering
    consoleLoop

consoleLoop = do
    c <- getChar
    if 'q' == c then exitSuccess else consoleLoop
