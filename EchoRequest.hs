module EchoRequest(EchoRequestTable,newEchoRequestTable,newEchoRequest) where

import NetAddress
import Request
import qualified Data.IntMap.Strict as M
import Control.Concurrent.MVar

data EchoResult = EchoSuccess | EchoFail NetAddress deriving Show
type EchoRequestTable = RequestTable EchoResult
newEchoRequestTable :: IO EchoRequestTable
newEchoRequestTable = newRequestTable

newEchoRequest :: EchoRequestTable -> Maybe Int -> IO (RqID, MVar (Maybe EchoResult))
newEchoRequest = newRequest
