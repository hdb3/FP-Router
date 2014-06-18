module RouterDVP where

import Message
import ParseConfig
import NetChan
import Control.Concurrent
import Control.Monad

router :: ControlChan -> [NetChan a] -> ConfigItem -> IO()
router cc ncx rc = do
  putStrLn "Hello from Router DVP"
  controlChannel <- openControlChan cc
  let recv = recvControlChan controlChannel
  forever $ do
    msg <- recv
    putStrLn "DVP - message received"
    print msg
  
