module NetMessage where

import DVPMessage
data NetMessage = EchoNM EchoMsg | DVPNM DVPMsg | InfoNM String deriving Show

data EchoMsg = EchoReq Int Int | EchoRsp Int Int | EchoND Int Int Int deriving Show
