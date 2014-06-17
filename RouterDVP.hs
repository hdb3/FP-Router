module RouterDVP where

import Message
import Control.Concurrent
import Control.Monad

router :: ControlChan -> IO()
router cc = do
  putStrLn "Hello from Router DVP"
  controlChannel <- openControlChan cc
  let recv = recvControlChan controlChannel
  forever $ do
    msg <- recv
    putStrLn "DVP - message received"
    print msg
  
