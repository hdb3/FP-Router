module Echo where

import RouteTable
import NetChan
import NetMessage
{- implement the echo message handling functionality -}

processEcho :: EchoMsg -> RouteTable -> IO ()
processEcho msg _ = putStrLn $ "echo msg " ++ " : " ++ show msg
