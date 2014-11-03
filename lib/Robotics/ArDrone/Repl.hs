module Robotics.ArDrone.Repl where

import Robotics.ArDrone.Control

import Control.Monad

import Control.Concurrent
import Network.Socket

import System.IO.Unsafe
import Data.IORef

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
        threadDelay 25000

    return ()

initDefaultDrone = initDrone "192.168.1.1"

now :: ArDroneMsg -> IO ()
now = writeIORef currentCommand . Just . toAtCommand

[takeOff, land, stop, fTrim, disableEmergency] = map now [TakeOff, Land,  Stop, FTrim, DisableEmergency]

[up, down, cw, ccw, front, back, left, right] = map (now .) [Up, Down, Clockwise, CounterClockwise, Front, Back, MoveLeft, MoveRight]
