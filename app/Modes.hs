module Modes where

import Options.Applicative 
import Text.Read

import ADN hiding(info)
import ADN.Modes
import ADN.Debug
import ADN.Dispatcher
import Warp
import Warp.Pipes
import Warp.Types

data DefaultTester = DefaultTester {
    dtUserNumber :: Int,
    dtWarpTTL :: Int,
    dtWarpTimeout :: Int,
    dtProtocol :: WProtocol}

tester (DefaultTester nD ttl timeout pidD) =
        Mode "test" "simulate a network with many users" (pure []) parseTest
    where
        parseTest = hsubparser (command "full"  (info (run fullGraphTest) $ progDesc "a fully-connected network: each user is in direct link with each other") <>
                               (command "chain" (info (run chainTest) $ progDesc "the users a connected along a chain: 1 <-> 2 <-> 3 ... ")))
        run test = f <$> parseN <*> parseTTL <*> parseTimeout <*> parseProtocol reader pidD
            where f n ttlM to pid = test n $ map (pipeline ttlM to pid) [1..]
        pipeline ttlM to pid i c = pure c >>= decoder >>= simpleWarp warp pid >>= pipeEnd (dbg "")
            where warp = Warp (WID i) ttlM to []

        reader = WProtocol <$> maybeReader readMaybe
        parseN = option auto (short 'n' <> metavar "N"  <> value nD <> showDefault <> help "number of users in the network") 
        parseTTL = (TTL <$> option auto (long "max-ttl" <> metavar "TTL"  <> value ttl <> showDefault <> help "maximum number of hop for search packets"))
        parseTimeout = option auto (long "timeout" <> metavar "SECONDS"  <> value timeout <> showDefault <> help "maximum time of inactivity before an edge is removed from the network graph")


runNoWarp :: [WarpMode] -> LinkMode
runNoWarp wmds = dspOutput auto 0 $ runWarp <$> manyModes wmds mode
    where
        mode = Mode "no-warp" "link directly a warp mode to the link layer" noRootCalls (pure ()) :: Mode ()
        runWarp :: ((), [WarpState -> DspOutput WProtocol WarpMsg]) -> PipeEnd Payload
        runWarp ((), outputs) c = do
            ws <- makeWarp $ Warp (WID 0) 0 0 []
            pure c >>= decoder >>= runDispatcher warpDispatcher (map ($ws) outputs)


debugWarp :: Int -> String -> WarpMode
debugWarp pid name = simpleWarpMode (WProtocol pid) $ Mode "debug-warp" "connect a debugger to Warp" noRootCalls run
    where 
        run = pipeEnd . dbg <$> parseName
        parseName = strOption (long "dbg-name" <> metavar "NAME" <> value name <> showDefault <> help "name of the debugger in the CLI")

debugLink :: Int -> String -> LinkMode
debugLink pid name = dspOutput auto pid $ Mode "debug-link" "connect a debugger to a network link" noRootCalls run
    where 
        run = pipeEnd . dbg <$> parseName
        parseName = strOption (long "dbg-name" <> metavar "NAME" <> value name <> showDefault <> help "name of the debugger in the CLI")

runDebugModes :: [LinkMode] -> ADNMode
runDebugModes lmds = runDebug <$> manyModes lmds mode
    where 
        mode = Mode "debug" "simulate a link to the network by a debugger" noRootCalls parseName
        runDebug :: (String, [DspOutput Int Payload])-> ADN ()
        runDebug (name, outputs) = pipeStart (dbg name) >>= runDispatcher byteDispatcher outputs
        parseName = strOption (long "dbg-name" <> metavar "NAME" <> value "net" <> showDefault <> help "name of the debugger in the CLI")



