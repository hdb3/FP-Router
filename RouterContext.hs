module RouterContext where

import ParseConfig
import Link
import NetMessage
import NetAddress
import RouteTable
import EchoRequest
import DVP

data RouterContext = RouterContext { rcName :: String , rcAddress :: NetAddress, rcConfig :: ConfigItem, rcLinks :: [LinkInterface NetMessage], routeTable :: RouteTable, rcDVPTable :: DVPTMVar, rcEchoRequests :: EchoRequestTable}
