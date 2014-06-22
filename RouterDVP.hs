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
import DVP

debug = putStrLn
-- data RouterContext = RouterContext { rcName :: String , rcAddress :: NetAddress, rcConfig :: ConfigItem, rcLinks :: [NetReg NetMessage], routeTable :: RouteTable}
router :: ControlChan -> [NetChan NetMessage] -> ConfigItem -> IO()
router cc ncx rc = do
  let ri = rIndex rc
  let localConfig = parseRouter (rConfig rc)
  routeTable <- newRouteTable []
  controlChannel <- openControlChan cc
  links <- forM ncx netRegister
  echoRequestTable <- newEchoRequestTable
  dvpTable <- newDVPTable

  let routerContext = RouterContext name address rc links routeTable dvpTable echoRequestTable where
      address = read $ nodeAttr "address" localConfig :: NetAddress
      name = nodeAttr "name" localConfig

  debug $ "Hello from Router DVP(" ++ show (rcName routerContext) ++ " at " ++ show (rcAddress routerContext) ++ ")"
        -- ++ "\n****************\nconfig source\n****************\n" ++ (rConfig rc) ++ "****************\n"
        -- ++ ( unlines $ map show localConfig )
        -- ++ "\n****************\n"

  mapM (\li -> forkIO $ ifProcess routerContext li) [0 .. (length links)-1] -- link -1 is the local link address (loopback)
  forkIO (timerProcess routerContext)
  forkIO (dvpTimerProcess routerContext)
  forever $ do
    msg <- recvControlChan controlChannel
    -- debug "DVP - control message received"
    -- print msg
    processControl msg controlChannel routerContext

processControl (Command tokens) cchan rc = do
    command tokens rc (\response -> sendControlChan cchan (Response response))

command ("show":obj:_) rc f = do
    case obj of
      "route" -> do
                  rt <- readMVar (routeTable rc)
                  f $ show rt
      "dvp"   -> do
                   dvp <- readMVar (rcDVPTable rc)
                   f $ show dvp
      s       -> f $ "Unknown object: " ++ s
  
command ("time":_) _ f = do
    t <- nowSeconds
    f $ "POSIX time-stamp: " ++ show t
  
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
    -- forM (rcLinks context) (\link -> netSend link (InfoNM ("Hello (" ++ show myRouter ++ ")")))
    threadDelay 5000000

dvpTimerProcess context = forever $ do
    let myRouter = rIndex.rcConfig $ context
    let myAddr = rcAddress context
    let dvpTable = rcDVPTable context
    now <- nowSeconds
    let hostRoute = DVPRoute myAddr linkLocal 0 now
    updateDVPRoute_ hostRoute dvpTable
    dvpVec <- getDVPVec dvpTable
    forM (rcLinks context) (\link -> netSend link (DVPNM dvpVec))
    threadDelay 5000000

ifProcess context li = do
   forever $ do
       let myChan = (!!) (rcLinks context) li
       -- let myRouter = rIndex.rcConfig $ context
       (msg,src) <- netRecv myChan
       protocolProcess msg li src context

protocolProcess (DVPNM msg) link src context = do
    -- debug $ "DVP msg from " ++ show src ++ "/lnk:" ++ show link ++ " : " ++ show msg
    let dvpTable = rcDVPTable context
    now <- nowSeconds
    updateFromDVPMessage_ msg link now dvpTable
    let rt = routeTable context
    updateRouteTable_ rt dvpTable

protocolProcess (InfoNM msg) link src _ = do
    debug $ "info msg from " ++ show src ++ "/lnk:" ++ show link ++ " : " ++ show msg

protocolProcess (EchoNM msg) _ src context = do
    processEcho msg context

-- protocolProcess unknownMsg link src _ = do
    -- debug $ "unknown msg type from " ++ show src ++ "/lnk:" ++ show link ++ " : " ++ show unknownMsg
