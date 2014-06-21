module RouterDVP where

import RouterContext
import ParseRouterConfig
import ControlMessage
import ParseConfig
import NetChan
import NetMessage
import NetAddress
import RouteTable
import Echo
import EchoRequest
import Control.Concurrent
import Control.Monad
import Data.List

debug = putStrLn
-- data RouterContext = RouterContext { rcName :: String , rcAddress :: NetAddress, rcConfig :: ConfigItem, rcLinks :: [NetReg NetMessage], routeTable :: RouteTable}
router :: ControlChan -> [NetChan NetMessage] -> ConfigItem -> IO()
router cc ncx rc = do
  let ri = rIndex rc
  let localConfig = parseRouter (rConfig rc)
  routeTable <- newRouteTable
  controlChannel <- openControlChan cc
  links <- forM ncx netRegister
  echoRequestTable <- newEchoRequestTable

  let routerContext = RouterContext name address rc links routeTable echoRequestTable where
      address = read $ nodeAttr "address" localConfig :: NetAddress
      name = nodeAttr "name" localConfig

  debug $ "Hello from Router DVP(" ++ show (rcName routerContext) ++ " at " ++ show (rcAddress routerContext) ++ ")"
        -- ++ "\n****************\nconfig source\n****************\n" ++ (rConfig rc) ++ "****************\n"
        -- ++ ( unlines $ map show localConfig )
        -- ++ "\n****************\n"

  mapM (\li -> forkIO $ ifProcess routerContext li) [0 .. (length links)-1]
  forkIO (timerProcess routerContext)
  forever $ do
    msg <- recvControlChan controlChannel
    debug "DVP - control message received"
    print msg
    processControl msg controlChannel routerContext

processControl (Command tokens) cchan rc = do
    -- putStrLn $ intercalate " / " tokens
    -- putStrLn $ intercalate tokens " / "
    -- response <- command tokens rc
    -- sendControlChan cchan (Response response)
    command tokens rc (\response -> sendControlChan cchan (Response response))
    -- sendControlChan cchan (Response "I can\'t do much either")

command ("ping":target:_) rc f = do
    let targetAddress = NetAddress $ read target
    request <- echoRequest rc targetAddress
    maybe (f "request rejected")
          (\rq -> do
                  result <- takeMVar rq
                  f $ maybe "request rejected" show result )
          request
  
timerProcess context = forever $ do
    let myRouter = rIndex.rcConfig $ context
    forM (rcLinks context) (\link -> netSend link (InfoNM ("Hello (" ++ show myRouter ++ ")")))
    threadDelay 5000000

ifProcess context li = do
   forever $ do
       let myLink = (!!) (rcLinks context) li
       let myRouter = rIndex.rcConfig $ context
       (msg,src) <- netRecv myLink
       -- debug $ "recv: " ++ show myRouter ++ "/" ++ show li ++ " : " ++ show msg
       protocolProcess msg src context

protocolProcess (InfoNM msg) src _ = do
    debug $ "info msg from " ++ show src ++ " : " ++ show msg

protocolProcess (EchoNM msg) src context = do
    processEcho msg context

protocolProcess unknownMsg src _ = do
    debug $ "unknown msg type from " ++ show src ++ " : " ++ show unknownMsg
