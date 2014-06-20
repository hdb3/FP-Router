module NetMessage where

import NetAddress
import DVPMessage
data NetMessage = EchoNM EchoMsg | DVPNM DVPMsg | InfoNM String deriving Show

data EchoMsg = EchoReq NetAddress NetAddress | EchoRsp NetAddress NetAddress | EchoND NetAddress NetAddress NetAddress deriving Show
