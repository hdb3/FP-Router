module Echo where

import RouterContext
import RouteTable
import NetChan
import NetMessage
import NetAddress
import Data.Maybe
{- implement the echo message handling functionality -}

processEcho :: EchoMsg -> RouterContext -> IO ()
-- processEcho :: EchoMsg -> NetAddress -> RouteTable -> [NetReg NetMessage] -> IO ()

-- data EchoMsg = EchoReq NetAddress NetAddress | EchoRsp NetAddress NetAddress | EchoND NetAddress NetAddress NetAddress deriving Show
--  process request
processEcho msg@(EchoReq src dest) rc | dest == (rcAddress rc) = do
                                                          putStrLn "echo request for this node received!!"
                                                          nextHop <- getRoute (routeTable rc) src
                                                          let link = (rcLinks rc) !! (fromJust nextHop)
                                                          let resp = EchoNM (EchoRsp src dest)
                                                          if isNothing nextHop then ( putStrLn "no route to return message!!")
                                                                               else ( netSend link resp)
{-
                                      | otherwise = do
                                                          nextHop <- getRoute rt dest
                                                          fromMaybe (putStrLn "no route to forward message!!")
                                                                    echoForward
                                                                    nextHop where
                                                                        echoForward = netSend (fromJust nextHop) (EchoNM msg)
-}

processEcho msg _ = putStrLn $ "echo msg " ++ " : " ++ show msg
