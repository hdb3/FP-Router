
import NetChan
import Control.Concurrent
import Control.Monad
import System.IO

echoClient chan = do
    myChan <- netRegister chan
    forever $ do
        (s,_) <- netRecv myChan
        netSend myChan $ reverse s

main = do
    chan <- newNetChan :: IO (NetChan String)
    forkIO $ echoClient chan
    myChan <- netRegister chan
    forever $ do 
        s <- getLine
        netSend myChan s
        (r,_) <- netRecv myChan
        putStrLn $ "received: " ++ r
