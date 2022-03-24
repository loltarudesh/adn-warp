module Main where

import ADN.Modes
import Warp
import Conf
import Modes


main = runADNModes [linkM,
                    debugM,
                    tester defaultTester]

debugM = runDebugModes linkModes
linkM = runLinkModes defaultLink linkModes

linkModes = [warpM, noWarpM, debugLink 0 "dbg"]

warpM = runWarpModes defaultWarp warpModes
noWarpM = runNoWarp warpModes

warpModes = [natClient defaultClient, natServer defaultServer, debugWarp 0 "wdbg"]

