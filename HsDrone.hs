{-# LANGUAGE NamedFieldPuns #-}

import Network.Socket
import Control.Concurrent
import Control.Monad

import Data.List

import Data.Binary.IEEE754
import Data.Int

import System.IO.Unsafe
import Data.IORef
import Debug.Trace

data ArDroneMsg = TakeOff
                | Land

                | Up Float
                | Down Float

                | Clockwise Float
                | CounterClockwise Float

                | Front Float
                | Back Float

                | MoveLeft Float
                | MoveRight Float

                | Stop
                | FTrim
                | Calibrate

                | Config String String

                | Animate String Int

                | DisableEmergency
                deriving (Show)

data AtCommand = AtRef String
               | AtFTrim
               | AtPCmd { enable :: Bool
                        , pitch :: Float
                        , roll :: Float
                        , gaz :: Float
                        , yaw :: Float }
               | AtConfig String String
               deriving (Show)

type SequenceNumber = Int

connectDrone :: String -> IO Socket
connectDrone ip = do
    addrInfos <- getAddrInfo Nothing (Just ip) (Just "5556")
    let serverAddr = head addrInfos

    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    connect sock (addrAddress serverAddr)

    return sock

runDrone :: String -> [(Int, ArDroneMsg)] -> IO ()
runDrone ip msgs = do
    sock <- connectDrone ip

    let commands :: [(SequenceNumber, AtCommand)]
        commands = zip [1..] . concat $ do
            (waitAfterward, msg) <- msgs
            -- how many times do we send this msg out?
            -- supposing we send it out every 30 ms
            let n = waitAfterward `div` 30

            return . replicate n $ toAtCommand msg

    forM_ commands $ \(num, command) -> do
        send sock $ traceId $ fromAtCommand command num
        threadDelay 30000

main = withSocketsDo $ do
    runDrone "192.168.1.1" $
        [ (6000, TakeOff)
        , (2000, CounterClockwise 0.5)
        , (2000, Land)
        ]

toAtCommand :: ArDroneMsg -> AtCommand
toAtCommand msg =
    case msg of
     TakeOff -> AtRef "290718208"
     Land -> AtRef "290717696"

     Up speed -> AtPCmd True 0 0 speed 0
     Down speed -> AtPCmd True 0 0 (-speed) 0

     -- fixme not sure which is which
     Clockwise speed -> AtPCmd True 0 0 0 speed
     CounterClockwise speed -> AtPCmd True 0 0 0 (-speed)

     Front speed -> AtPCmd True 0 (-speed) 0 0
     Back speed -> AtPCmd True 0 speed 0 0

     MoveLeft speed -> AtPCmd True 0 speed 0 0
     MoveRight speed -> AtPCmd True 0 (-speed) 0 0

     Stop -> AtPCmd False 0 0 0 0

     FTrim -> AtFTrim
     DisableEmergency -> AtRef "290717952"

fromAtCommand :: AtCommand -> Int -> String
fromAtCommand cmd num =
    case cmd of
     AtRef param -> "AT*REF=" ++ show num ++ "," ++ param ++ "\r"

     AtPCmd { enable, pitch, roll, gaz, yaw } ->
         let enableNum = if enable then "1" else "0"
             suffix = intercalate "," . map (show . floatToInt) $
                      [pitch, roll, gaz, yaw]
         in "AT*PCMD=" ++ show num ++ "," ++
            enableNum ++ "," ++ suffix ++ "\r"

     AtFTrim ->
         "AT*FTRIM=" ++ show num

floatToInt :: Float -> Int32
floatToInt = fromIntegral . floatToWord

-- FOR REPL
-----------
currentCommand :: IORef (Maybe AtCommand)
{-# NOINLINE currentCommand #-}
currentCommand = unsafePerformIO (newIORef Nothing)

-- for REPL / interleave with other IO
initDrone :: String -> IO ()
initDrone ip = do
    sock <- connectDrone ip

    forkIO . forM_ [1..] $ \num -> do
        currentCommandM <- readIORef currentCommand
        case currentCommandM of
         Nothing -> return ()
         Just cmd -> do send sock $ fromAtCommand cmd num
                        return ()

    return ()

initDefaultDrone = initDrone "192.168.1.1"

now :: ArDroneMsg -> IO ()
now = writeIORef currentCommand . Just . toAtCommand

[takeOff, land, stop, fTrim, disableEmergency] = map now [TakeOff, Land,  Stop, FTrim, DisableEmergency]

[up, down, cw, ccw, front, back, left, right] = map (now .) [Up, Down, Clockwise, CounterClockwise, Front, Back, MoveLeft, MoveRight]
