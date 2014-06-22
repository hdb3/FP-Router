module DVP where
import qualified Data.IntMap as M
import NetAddress
import RouteTable
import DVPMessage
import Data.Time.Clock.POSIX
import Control.Concurrent.MVar

data DVPRoute = DVPRoute { addr :: NetAddress, link :: Int, cost :: Int, age :: Int } deriving Show

type Seconds = Int
type DVPTable = M.IntMap DVPRoute
type DVPTMVar = MVar DVPTable
apply :: (DVPTable -> DVPTable) -> DVPTMVar -> IO()
apply f dvptm = do
    dvpt <- takeMVar dvptm
    putMVar dvptm (f dvpt)

newDVPTable :: IO DVPTMVar
newDVPTable = newMVar M.empty

nowSeconds :: IO Seconds
nowSeconds = do
    pt <- getPOSIXTime
    return $ truncate pt

updateDVPRoute_ :: DVPRoute -> DVPTMVar -> IO ()
-- updateDVPRoute_ route dvptm = takeMVar dvptm >>= putMVar dvptm . updateDVPRoute route
updateDVPRoute_ a1 = apply $ updateDVPRoute a1

updateDVPRoute :: DVPRoute -> DVPTable -> DVPTable
updateDVPRoute route table = M.insertWith f k route table where
    k = fromNetAddress $ addr route
    f new old = if cost new < cost old then new else old

updateFromDVPMessage_ :: DVPMsg -> IfIndex -> Seconds -> DVPTMVar -> IO ()
updateFromDVPMessage_ m i t = apply $ updateFromDVPMessage m i t

updateFromDVPMessage :: DVPMsg -> IfIndex -> Seconds -> DVPTable -> DVPTable
updateFromDVPMessage dvpvec link time dvpt = foldl (update link time) dvpt dvpvec where
    update :: IfIndex -> Seconds -> DVPTable -> DVPVec -> DVPTable
    update link time tab (DVPVec addr cost) = updateDVPRoute (DVPRoute addr link (cost+1) time) tab

vectorsFromDVP :: DVPTable -> DVPMsg
vectorsFromDVP dvpt = map (\dvpr -> DVPVec (addr dvpr) (cost dvpr)) (M.elems dvpt)

getDVPVec :: DVPTMVar -> IO DVPMsg
getDVPVec dvptmv = readMVar dvptmv >>= return . vectorsFromDVP

routesFromDVP :: DVPTable -> [(NetAddress,IfIndex)]
routesFromDVP dvpt = map (\dvpr -> (addr dvpr, link dvpr)) (M.elems dvpt)

updateRouteTable_ :: RouteTable -> DVPTMVar -> IO ()
updateRouteTable_  rt dvptm = readMVar dvptm >>= updateRouteTable rt

updateRouteTable :: RouteTable -> DVPTable -> IO ()
updateRouteTable rt dvpt = rebuildRouteTable rt (routesFromDVP dvpt)
