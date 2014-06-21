module NetMessage where

import NetAddress
import DVPMessage
import Request
data NetMessage = EchoNM EchoMsg | DVPNM DVPMsg | InfoNM String deriving Show

data EchoMsg = EchoReq RqID NetAddress NetAddress | EchoRsp RqID NetAddress NetAddress | EchoND RqID NetAddress NetAddress NetAddress deriving Show
