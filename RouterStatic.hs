module RouterStatic where

import ControlMessage
import ParseConfig
import NetChan
import Control.Concurrent
import Control.Monad

router :: ControlChan -> [NetChan a] -> ConfigItem -> IO()
router cc ncx rc = do
  putStrLn "Hello from RouterStatic"
  controlChannel <- openControlChan cc
  let recv = recvControlChan controlChannel
  forever $ do
    msg <- recv
    putStrLn "Static - message received"
    print msg
  
