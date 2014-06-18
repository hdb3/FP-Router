
import NetChan
import Control.Concurrent
import Control.Monad
import System.IO
import System.Environment


echoClient start limit chan1 flag = do
    myId <- myThreadId
    putStrLn $ "Echo Client " ++ (if start then "START" else "WAIT") ++ " id: " ++ show myId
    myChan1 <- netRegister chan1
    when start (netSend myChan1 0)
    forever $ do
        (n,id) <- netRecv myChan1
        when (n>limit)
             (putMVar flag ())
        netSendTo myChan1 id (n+1)

main = do
    args <- getArgs
    -- limit <- if (null args) then readLn else return $ read (args !! 0)
    limit <- if (null args)
             then do
                 putStr "repeat count? "
                 hFlush stdout
                 count <- readLn
                 return count
              else return $ read (args !! 0)
    endMVar <- newEmptyMVar :: IO (MVar ())
    chan1 <- newNetChan :: IO (NetChan Int)
    forkIO $ echoClient False limit chan1 endMVar
    forkIO $ echoClient True limit chan1 endMVar
    takeMVar endMVar
    putStrLn "main exiting"
