module RouterDVP where

import Message
import ParseConfig
import NetChan
import NetMessage
import RouteTable
import Control.Concurrent
import Control.Monad

data RouterContext = RouterContext {rcConfig :: ConfigItem, rcLinks :: [NetReg NetMessage], routeTable :: RouteTable}
router :: ControlChan -> [NetChan NetMessage] -> ConfigItem -> IO()
router cc ncx rc = do
  let ri = rIndex rc
  putStrLn $ "Hello from Router DVP(" ++ show ri ++ ")"
  routeTable <- newRouteTable
  controlChannel <- openControlChan cc
  let recv = recvControlChan controlChannel

  links <- forM ncx netRegister
  let routerContext = RouterContext rc links routeTable
  mapM (\li -> forkIO $ ifProcess routerContext li) [0 .. (length links)-1]
  forkIO (timerProcess routerContext)
  forever $ do
    msg <- recv
    putStrLn "DVP - control message received"
    print msg
  
timerProcess context = forever $ do
    let myRouter = rIndex.rcConfig $ context
    forM (rcLinks context) (\link -> netSend link (InfoNM ("Hello (" ++ show myRouter ++ ")")))
    threadDelay 5000000

ifProcess context li = do
   forever $ do
       let myLink = (!!) (rcLinks context) li
       let myRouter = rIndex.rcConfig $ context
       (msg,src) <- netRecv myLink
       -- putStrLn $ "recv: " ++ show myRouter ++ "/" ++ show li ++ " : " ++ show msg
       protocolProcess msg src context

protocolProcess (InfoNM msg) src _ = do
    putStrLn $ "info msg from " ++ show src ++ " : " ++ show msg

protocolProcess (EchoNM msg) src context = do
    putStrLn $ "echo msg from " ++ show src ++ " : " ++ show msg

protocolProcess unknownMsg src _ = do
    putStrLn $ "unknown msg type from " ++ show src ++ " : " ++ show unknownMsg
