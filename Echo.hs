module Echo where

import RouterContext
import RouteTable
import NetChan
import NetMessage
import NetAddress
import Data.Maybe
{- implement the echo message handling functionality -}

echoRequest :: RouterContext -> NetAddress -> IO ()
echoRequest _ dest = putStrLn $ "ping for " ++ show dest

processEcho :: EchoMsg -> RouterContext -> IO ()

--  process request
processEcho msg@(EchoReq src dest) rc
     | dest == (rcAddress rc) = do
          putStrLn "echo request for this node received!!"
          nextHop <- getRoute (routeTable rc) src
          let link = (rcLinks rc) !! (fromJust nextHop)
          let resp = EchoNM (EchoRsp src dest)
          if isNothing nextHop
              then ( putStrLn "no route to return message!!")
              else ( netSend link resp)
     | otherwise = do
          putStrLn "echo request for another node received!!"
          nextHop <- getRoute (routeTable rc) dest
          let link = (rcLinks rc) !! (fromJust nextHop)
          if isNothing nextHop
              then  (do putStrLn "no route to forward message!!"
                        nextHop <- getRoute (routeTable rc) src
                        let retlink = (rcLinks rc) !! (fromJust nextHop)
                        if isNothing nextHop
                            then  (putStrLn "no route to return failure message either!!")
                            else (netSend retlink ( EchoNM (EchoND src dest (rcAddress rc)))))
              else netSend link (EchoNM msg)

-- data EchoMsg = EchoReq NetAddress NetAddress | EchoRsp NetAddress NetAddress | EchoND NetAddress NetAddress NetAddress deriving Show
--  process request
processEcho msg@(EchoRsp src dest) rc
     | dest == (rcAddress rc) = do
          putStrLn "echo response for this node received!!"
     | otherwise = do
          putStrLn "echo response for another node received!!"
          nextHop <- getRoute (routeTable rc) dest
          let link = (rcLinks rc) !! (fromJust nextHop)
          if isNothing nextHop
              then  (putStrLn "no route to return message!!")
              else netSend link (EchoNM msg)

processEcho msg _ = putStrLn $ "echo msg " ++ " : " ++ show msg
