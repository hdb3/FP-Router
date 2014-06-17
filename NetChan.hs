module NetChan where

import Control.Monad
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
type NetChannel a = M.Map LinkAddress (NetQueue a)
type NetChan a = MVar (NetChannel a)
type NetReg a = (NetChan a,NetQueue a,LinkAddress)

netSend :: NetReg a -> a -> IO()
netSend (nc,q,id) msg = do
    m <- readMVar nc
    let qs = M.elems m
    let send dest = unless (q==dest) (writeChan dest (LinkMsg id msg))
    mapM_ send qs

netRecv :: NetReg a -> IO (a,LinkAddress)
netRecv (_,q,_) = do
     LinkMsg la msg <- readChan q
     return (msg,la)
    
newNetChan :: IO (NetChan a)
newNetChan = do
    chan <- newMVar M.empty
    return chan

netRegister :: NetChan a -> IO (NetReg a)
netRegister netChan = do
    id <- myThreadId
    chan <- newChan
    m <- takeMVar netChan
    let m' = M.insert id chan m
    putMVar netChan m'
    return (netChan,chan,id)
