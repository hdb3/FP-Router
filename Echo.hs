module Echo where

import RouteTable
import NetChan
import NetMessage
import NetAddress
{- implement the echo message handling functionality -}

processEcho :: EchoMsg -> NetAddress -> RouteTable -> IO ()
processEcho msg _ _ = putStrLn $ "echo msg " ++ " : " ++ show msg
