module RouterDVP where

import Message
import ParseConfig
import NetChan
import NetMessage
import Control.Concurrent
import Control.Monad

-- router :: ControlChan -> [NetChan a] -> ConfigItem -> IO()
router :: ControlChan -> [NetChan NetMessage] -> ConfigItem -> IO()
router cc ncx rc = do
  let ri = rIndex rc
  putStrLn $ "Hello from Router DVP(" ++ show ri ++ ")"
  controlChannel <- openControlChan cc
  let recv = recvControlChan controlChannel

  links <- forM ncx netRegister
  mapM (\link -> forkIO $ ifProcess ri link) links
  forkIO (timerProcess ri links)
  forever $ do
    msg <- recv
    putStrLn "DVP - control message received"
    print msg
  
timerProcess ri links = forever $ do
    forM links (\link -> netSend link (InfoNM ("Hello (" ++ show ri ++ ")")))
    threadDelay 5000000

ifProcess ri nc = do
   forever $ do
       (msg,src) <- netRecv nc 
       putStrLn $ "recv: " ++ show ri ++ " : " ++ show msg
