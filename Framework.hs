
import System.Environment(getArgs)
import System.IO
import System.Exit
import qualified Data.Map as M
import Control.Concurrent
import Message
import Config
import RouterDVP
import RouterStatic

framework routers links = do
    putStrLn "Hello from Framework"
    putStrLn $ "I have " ++ show (length routers) ++ " routers and " ++ show (M.size links) ++ " links."

createLink li st rix = do
    chan <- 
startRouter (RouterConfig ri rt rlnks conf state) = do
    chan <- newControlChan
    f = case rt of
       Static -> RouterStatic.router
       DVP -> RouterDVP.router
    thread <- forkIO $ f chan


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
