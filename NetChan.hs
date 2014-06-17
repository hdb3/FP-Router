module NetChan where

import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Map as M

{-
  this is an implementation of a broadcast network with unicast and broadcast facilities
  it uses the standard Concurrent.Chan channel structures with an addition routing map which allows individual directed messages to be
  delivered to a single destination, and also broadcast messages to be copied to all interested receivers.
  The API extension is on the send call, where an optional destination address can be supplied.
  Additionally, link peers must register on the channel in order to be able to use it.
-}

data LinkMsg a = LinkMsg LinkAddress a
type NetQueue a = Chan (LinkMsg a)
type LinkAddress = ThreadId
data NetChannel a = NetChannel { chanMap :: M.Map LinkAddress (NetQueue a) }
type NetChan a = MVar (NetChannel a)
type NetReg a = (NetChan a,NetQueue a)

getLinkAddress :: LinkMsg a -> LinkAddress
getLinkAddress (LinkMsg la _) = la
netSend :: NetChan a -> a -> IO()
netSend chan msg = do
    return ()
netRecv :: NetChan a -> IO a
netRecv chan = do
    return "rubbish"
newNetChan :: IO (NetChan a)
newNetChan = do
    chan <- newMVar (NetChannel M.empty)
    return chan

netRegister :: NetChan a -> IO (NetReg a)
netRegister netChan = do
    id <- myThrreadId
    chan <- newChan
    m <- takeMVar netChan
    let m' = 
    return(
