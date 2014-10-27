import Network.Socket
import Control.Concurrent
import Control.Monad

takeOff s num = send s $ "AT*REF=" ++ show num ++ ",290718208\r"

land s num = send s $ "AT*REF=" ++ show num ++ ",290717696\r"

main = withSocketsDo $ do
    addrInfos <- getAddrInfo Nothing (Just "192.168.1.1") (Just "5556")
    let serverAddr = head addrInfos

    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect sock (addrAddress serverAddr)

    forM_ [1..167] $ \num -> do -- 5 seconds
        threadDelay 30000 -- 30 ms
        takeOff sock num

    forM_ [168..168*2] $ \num -> do
        threadDelay 30000
        land sock num
