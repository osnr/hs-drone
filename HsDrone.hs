{-# LANGUAGE NamedFieldPuns #-}

import Network.Socket
import Control.Concurrent
import Control.Monad

import Data.List

import Data.Binary.IEEE754
import Data.Int

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
                | Calibrate

                | Config String String

                | Animate String Int

                | DisableEmergency
                deriving (Show)

data AtCommand = AtRef String
               | AtFTrim
               | AtPCmd { pitch :: Float
                        , roll :: Float
                        , gaz :: Float
                        , yaw :: Float }
               | AtConfig String String
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

     Up speed -> AtPCmd 0 0 speed 0
     Down speed -> AtPCmd 0 0 (-speed) 0

     -- fixme not sure
     Clockwise speed -> AtPCmd 0 0 0 speed
     CounterClockwise speed -> AtPCmd 0 0 0 (-speed)

     Front speed -> AtPCmd 0 (-speed) 0 0
     Back speed -> AtPCmd 0 speed 0 0

     MoveLeft speed -> AtPCmd 0 speed 0 0
     MoveRight speed -> AtPCmd 0 (-speed) 0 0

     DisableEmergency -> AtRef "290717952"

fromAtCommand :: AtCommand -> Int -> String
fromAtCommand cmd num =
    case cmd of
     AtRef param -> "AT*REF=" ++ show num ++ "," ++ param ++ "\r"
     AtPCmd { pitch, roll, gaz, yaw } ->
         let suffix = intercalate "," . map (show . floatToInt) $
                      [pitch, roll, gaz, yaw]
         in "AT*PCMD=" ++ show num ++ ",1," ++ suffix ++ "\r"

floatToInt :: Float -> Int32
floatToInt = fromIntegral . floatToWord
