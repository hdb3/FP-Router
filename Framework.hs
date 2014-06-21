
import System.Environment(getArgs)
import System.IO
import System.Exit
import qualified Data.Map as M
import Control.Concurrent
import Control.Monad
import Data.Maybe
import ControlMessage
import Config
import RouterDVP
import RouterStatic
import NetChan
import Console

framework flag routers links = do
    putStrLn "Hello from Framework"
    putStrLn $ "I have " ++ show (length routers) ++ " routers and " ++ show (M.size links) ++ " links."
    rtable <- mapM (startRouter links) routers
    console (local rtable) (remote rtable)
    -- takeMVar flag
    putMVar flag ()

startRouter linkMap rc@(RouterConfig ri rt rlnks conf state) = do
    chan <- newControlChan
    let f = case rt of
                Static -> RouterStatic.router
                DVP -> RouterDVP.router
    let links = map (\k -> snd $ linkMap M.! k) rlnks
    thread <- forkIO $ f chan links rc
    return (thread,chan,ri)

local :: [(ThreadId, ControlChan, Int)] -> [String] -> IO String
local rtable command = return "I can\'t do a lot right now!"

remote :: [(ThreadId, ControlChan, Int)] -> [String] -> Int -> IO String
remote rtable command target = do
    let channel = getRemote rtable target
    maybe (return "router instance not found!") sendAndReceive channel where
        getRemote :: [(ThreadId, ControlChan, Int)] -> Int -> Maybe ControlChan
        getRemote [] _ = Nothing
        getRemote ((t,c,r):tcrs) ri = if r==ri then Just c else getRemote tcrs ri
        sendAndReceive chan = do
            sendControlChan chan (Command command)
            Response msg <- recvControlChan chan
            return msg

main = do
    args <- getArgs
    let arg1 = if null args then "config.txt" else args !! 0
    putStrLn $ "Using file " ++ arg1 ++ " for configuration"
    (routers,links) <- fileConfig arg1

    linkList <- forM (M.toList links) (\(k,(st,_)) -> do chan <- newNetChan
                                                         return (k,(st,chan)) )

    let linkMap = M.fromList linkList
    flag <- newEmptyMVar :: IO (MVar ())
    frameworkThread <- forkIO ( framework flag routers linkMap)
    readMVar flag
{-
    hSetBuffering stdin NoBuffering
    consoleLoop flag
-}
{-
    consoleLoop where
        consoleLoop = do
            c <- getChar
            if 'q' == c then ( putMVar flag () >> takeMVar flag) else consoleLoop
-}
consoleLoop flag = do
    c <- getChar
    if 'q' == c then ( putMVar flag () >> takeMVar flag) else consoleLoop flag
