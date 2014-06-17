module RouterStatic where

import Message
import Control.Concurrent
import Control.Monad

router :: ControlChan -> IO()
router cc = do
  putStrLn "Hello from RouterStatic"
  controlChannel <- openControlChan cc
  let recv = recvControlChan controlChannel
  forever $ do
    msg <- recv
    putStrLn "Static - message received"
    print msg
  
