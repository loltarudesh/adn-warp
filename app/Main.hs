module Main where

import ADN.Modes
import Warp
import Conf
import Modes


main :: IO ()
main = runADNModes [linkM,
                    debugM,
                    tester defaultTester]


debugM :: ADNMode
debugM = runDebugModes linkModes

linkM :: ADNMode
linkM = runLinkModes defaultLink linkModes

linkModes :: [LinkMode]
linkModes = [warpM, noWarpM, debugLink 0 "dbg"]

warpM :: LinkMode
warpM = runWarpModes defaultWarp warpModes

noWarpM :: LinkMode
noWarpM = runNoWarp warpModes

warpModes :: [WarpMode]
warpModes = [natClient defaultClient, natServer defaultServer, debugWarp 0 "wdbg"]

