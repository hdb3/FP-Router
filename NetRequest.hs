module NetRequest(newMsgRequestTable,newMsgRequest) where

import NetMessage
import Request
import qualified Data.IntMap.Strict as M
import Control.Concurrent.MVar

type NetRequestTable = RequestTable NetMessage
newMsgRequestTable :: IO NetRequestTable
newMsgRequestTable = newRequestTable

newMsgRequest :: NetRequestTable -> Maybe Int -> IO (MVar (Maybe NetMessage))
newMsgRequest = newRequest
