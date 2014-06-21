module Request where

import qualified Data.IntMap.Strict as M
import Control.Concurrent.MVar
import Data.Time

newtype RqID = RqID Int deriving (Show,Eq)

type RequestTable a = MVar (RqID, M.IntMap (Maybe UTCTime, MVar (Maybe a)))

newRequestTable :: IO (RequestTable a)
newRequestTable = newMVar (RqID 0 , M.empty)

newRequest :: RequestTable a -> Maybe Int -> IO (RqID, MVar (Maybe a))
newRequest rqt timeout = do
    time <- getCurrentTime
    let expiryTime = maybe Nothing (\t -> Just $ addUTCTime (fromIntegral t / 1000000) time) timeout
    mv <- newEmptyMVar
    (RqID id,m) <- takeMVar rqt
    let newMap = M.insert id (expiryTime, mv) m
    let newID = RqID (id + 1)
    putMVar rqt (newID , newMap)
    return (RqID id,mv)

completeRequest :: RequestTable a -> RqID -> Maybe a -> IO ()
completeRequest rqt (RqID id) val = do
    all@(nextID,table) <- takeMVar rqt
    maybe (putMVar rqt all)
          (\(_,mv) -> do
              putMVar mv val
              putMVar rqt (nextID, M.delete id table)
          )
          ( M.lookup id table )
