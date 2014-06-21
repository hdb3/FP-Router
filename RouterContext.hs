module RouterContext where

import ParseConfig
import NetChan
import NetMessage
import NetAddress
import RouteTable

data RouterContext = RouterContext { rcName :: String , rcAddress :: NetAddress, rcConfig :: ConfigItem, rcLinks :: [NetReg NetMessage], routeTable :: RouteTable}
