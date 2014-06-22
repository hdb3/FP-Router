module RouteTable where

import NetAddress
import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
{-
  the route table is instantiated for every router instance.
  routing protocols update and may read it to determine routing information to propagate to other routers
  it is also used by the RI to determine how to forward echo msgs, and by the forwarding engine to forward user data

  it is a persistent, mutable data structure whose external interface is defined by the API:
  operations on the route table include:
  - request the next hop for a given destination
  - insert a new route - this consists of a pair - (next hop, destination)
  - delete a route
  - update a route
  in this initial simple implementation there can only be one route to a given destination,  so the insert and update operations are equivalent

  note: the route table is probably not the data structure used by a routing protocol to hold all of its state
        however, for a static router it is all that is required

-}

type IfIndex = Int
linkLocal = (-1) :: IfIndex
type RouteTable = MVar (M.Map NetAddress IfIndex)
type Route = (NetAddress,IfIndex)

newRouteTable :: [(NetAddress,IfIndex)] -> IO RouteTable
newRouteTable [] = newMVar M.empty
newRouteTable routes = newMVar (M.fromList routes)

rebuildRouteTable :: RouteTable -> [(NetAddress,IfIndex)] -> IO ()
rebuildRouteTable rtm routes = do
    rt <- takeMVar rtm
    putMVar rtm (M.fromList routes)

addRoute :: RouteTable -> NetAddress -> IfIndex -> IO ()
addRoute table addr ifindex = do
    map <- takeMVar table
    putMVar table (M.insert addr ifindex map)

delRoute :: RouteTable -> NetAddress -> IO ()
delRoute table addr = do
    map <- takeMVar table
    putMVar table (M.delete addr map)

getRoute :: RouteTable -> NetAddress -> IO (Maybe IfIndex)
getRoute table addr = do
    map <- readMVar table
    return (M.lookup addr map)
