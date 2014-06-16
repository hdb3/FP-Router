module Message where
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

openControlChan :: ControlChan -> ControlChan
openControlChan (ControlChan sc rc) = ControlChan rc sc

sendControlChan :: ControlChan -> ControlMsg -> IO ()
sendControlChan chan = writeChan (sendCChan chan)

recvControlChan :: ControlChan -> IO ControlMsg
recvControlChan chan = readChan (recvCChan chan)

data ControlMsg = ControlStart | ControlStop | ControlConfig String | ControlLinkInsert NetworkChan
-- newtype ChanC = Chan ControlMsg
data ControlChan = ControlChan { sendCChan :: Chan ControlMsg , recvCChan :: Chan ControlMsg }
-- data ControlChan = ControlChan { sendCChan :: ChanC , recvCChan :: ChanC }


-- network channels

-- newtype ChanN = Chan NetworkMsg
-- data NetworkChan = NetworkChan { sendChan :: ChanN , recvChan :: ChanN }
data NetworkChan = NetworkChan { sendChan :: Chan NetworkMsg , recvChan :: Chan NetworkMsg }
data NetworkMsg = EchoReq Int Int | EchoRsp Int Int

newNetworkChan :: IO (NetworkChan)
newNetworkChan = do
    sc <- newChan
    rc <- newChan
    return $ NetworkChan sc rc

openNetworkChan :: NetworkChan -> IO NetworkChan
openNetworkChan (NetworkChan sc rc) = do
    sc' <- dupChan sc
    return $ NetworkChan rc sc'

sendNetworkChan :: NetworkChan -> NetworkMsg -> IO ()
sendNetworkChan chan = writeChan (sendChan chan)

recvNetworkChan :: NetworkChan -> IO NetworkMsg
recvNetworkChan chan = readChan (recvChan chan)
