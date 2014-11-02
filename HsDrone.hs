import Network.Socket
import Control.Concurrent
import Control.Monad

data ArDroneMsg = TakeOff | Land
                deriving (Show)

data AtCommand = AtRef String
               deriving (Show)

type SequenceNumber = Int

runDrone :: String -> [(Int, ArDroneMsg)] -> IO ()
runDrone ip msgs = do
    addrInfos <- getAddrInfo Nothing (Just ip) (Just "5556")
    let serverAddr = head addrInfos

    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect sock (addrAddress serverAddr)

    let commands :: [(SequenceNumber, AtCommand)]
        commands = zip [1..] . concat $ do
            (waitAfterward, msg) <- msgs
            -- how many times do we send this msg out?
            -- supposing we send it out every 30 ms
            let n = traceShow (waitAfterward `div` 30) $ (waitAfterward `div` 30)

            return . replicate n $ toAtCommand msg

    forM_ commands $ \(num, command) -> do
        send sock $ fromAtCommand command num
        threadDelay 30000

main = withSocketsDo $ do
    runDrone "192.168.1.1" $
        [ (3000, TakeOff)
        , (3000, Land)
        ]

toAtCommand :: ArDroneMsg -> AtCommand
toAtCommand msg =
    case msg of
     TakeOff -> AtRef "290718208"
     Land -> AtRef "290717696"

fromAtCommand :: AtCommand -> Int -> String
fromAtCommand cmd num =
    case cmd of
     AtRef param -> "AT*REF=" ++ show num ++ "," ++ param ++ "\r"
