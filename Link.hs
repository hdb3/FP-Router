module Link where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe
import qualified Data.Map as M

{-
  this is an implementation of a broadcast network providing both unicast and broadcast sending operations
  it uses the standard Concurrent.Chan channel structures with an addition routing map which allows individual directed messages to be
  delivered to a single destination, whilst broadcast messages are copied to all registered receivers.
  Once attached it provides an equivalent service to a Ethernet LAN
  link peers must register/attach on the channel in order to be able to use it.  This operation sets up the underlying Concurrent Haskell
  framework.
-}

data LinkMsg a = LinkMsg LinkAddress a
type LinkAddress = ThreadId
type LinkQueue a = Chan (LinkMsg a) -- each attached device has its own queue...
                                   -- Chan is defined in Control.Concurrent...
type LinkChannel_ a = M.Map LinkAddress (LinkQueue a) -- all of the channels attached to the link
type LinkChannel a = MVar (LinkChannel_ a)
type LinkInterface a = (LinkChannel a,LinkQueue a,LinkAddress) -- this is the object which clients use

linkSend :: LinkInterface a -> a -> IO()
linkSend (nc,q,id) msg = do
    m <- readMVar nc
    let qs = M.elems m
    let send dest = unless (q==dest) (writeChan dest (LinkMsg id msg))
    mapM_ send qs

linkSendTo :: LinkInterface a -> LinkAddress -> a -> IO()
linkSendTo (nc,_,id) la msg = do
    m <- readMVar nc
    let q = M.lookup la m
    unless (isNothing q)
           (writeChan (fromJust q) (LinkMsg id msg))

linkRecv :: LinkInterface a -> IO (a,LinkAddress)
linkRecv (_,q,_) = do
     LinkMsg la msg <- readChan q
     return (msg,la)
    
newLinkChannel :: IO (LinkChannel a)
newLinkChannel = do
    chan <- newMVar M.empty
    return chan

linkAttach :: LinkChannel a -> IO (LinkInterface a)
linkAttach netChan = do
    id <- myThreadId
    chan <- newChan
    m <- takeMVar netChan
    let m' = M.insert id chan m
    putMVar netChan m'
    return (netChan,chan,id)
