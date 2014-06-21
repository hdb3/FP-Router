module ControlMessage where
import Control.Concurrent.Chan

{-
  define the message types which may be sent on channels
  two types of channels are defined:
  * control channels for Framework <-> RI operations
  * network channels, which connect RIs and represent links

  Control channels are only ever 1:1, network channels may have any number of receivers (including zero...)
  In both cases the channels are bi-directional, i.e. consist of two (or more) Concurrent Haskell channels.

  The initial implmentation uses unbounded FIFOs, and does not address any issues such as flow control or queue length management

  Message types

  Control messages are the currency of the control channels: network channels carry protocol specific messages - for simplicity we
  define a generic protocol message type which encompasses all the protocol messages known
-}

newControlChan :: IO (ControlChan)
newControlChan = do
    sc <- newChan
    rc <- newChan
    return $ ControlChan sc rc

openControlChan :: ControlChan -> IO ControlChan
openControlChan (ControlChan sc rc) = return $ ControlChan rc sc

sendControlChan :: ControlChan -> ControlMsg -> IO ()
sendControlChan chan = writeChan (sendCChan chan)

recvControlChan :: ControlChan -> IO ControlMsg
recvControlChan chan = readChan (recvCChan chan)

data ControlMsg = ControlStart | ControlStop deriving Show
data ControlChan = ControlChan { sendCChan :: Chan ControlMsg , recvCChan :: Chan ControlMsg }
instance Show ControlChan where
    show _ = "ControlChan"
