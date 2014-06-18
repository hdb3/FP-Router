
import NetChan
import Control.Concurrent
import Control.Monad
import System.IO

limit = 1000

echoClient start chan1 chan2 = do
    id <- myThreadId
    putStrLn $ "Echo Client " ++ (if start then "START" else "WAIT") ++ " id: " ++ show id
    myChan1 <- netRegister chan1
    myChan2 <- netRegister chan2
    when start (netSend myChan1 0)
    forever $ do
        (n,id) <- netRecv myChan1
        when ((n `mod` 100) == 0)
             (putStrLn $ "client " ++ show id ++ " " ++ show n)
        when (n>limit)
             (netSend myChan2 ())
        netSend myChan1 (n+1)
        -- netSendTo myChan1 id (n+1)

main = do
    chan1 <- newNetChan :: IO (NetChan Int)
    chan2 <- newNetChan :: IO (NetChan ())
    forkIO $ echoClient False chan1 chan2
    forkIO $ echoClient True chan1 chan2
    myChan <- netRegister chan2
    netRecv myChan
    putStrLn "main exiting"
