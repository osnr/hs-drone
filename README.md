Haskell library to control AR Drone quadcopters. Loosely based on [node-ar-drone](https://github.com/felixge/node-ar-drone). Still a work in progress.

`Repl.hs` uses a couple hacks to make `ghci` a control REPL like the node one. To use it, run `ghci`, import the module, connect to the drone, and then play around:
```
Prelude> import Robotics.ArDrone.Repl
Prelude Robotics.ArDrone.Repl> initDefaultDrone
Prelude Robotics.ArDrone.Repl> takeOff
Prelude Robotics.ArDrone.Repl> cw 0.2
Prelude Robotics.ArDrone.Repl> stop
Prelude Robotics.ArDrone.Repl> land
```
