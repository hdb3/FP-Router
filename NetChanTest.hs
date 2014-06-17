
import NetChan
import Control.Concurrent
import Control.Monad
import System.IO

echoClient chan = do
    myChan <- netRegister chan
    forever $ do
        s <- netRecv myChan
        netSend myChan $ reverse s

main = do
    chan <- newNetChan :: NetChan String
    forkIO $ echoClient chan
    myChan <- netRegister chan
    forever $ do 
        s <- getLine
        netSend myChan s
        r <- netRecv myChan
        putStrLn $ "received: " ++ r
