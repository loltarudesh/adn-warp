module Warp.Modes.Warp (
    -- * Warp modes
    WarpMode,
    runWarpModes,

    -- * building warp modes
    makeWarpMode,
    simpleWarpMode,

    -- * Parsing arguments
    parseWarp,

    -- * Default structure
    DefaultWarp(..),
    )where

import Options.Applicative 
import Text.Read

import Warp.Pipes
import Warp.Types
import Warp.Modes.Link

import ADN.Core hiding (info)
import ADN.Modes
import ADN.Dispatcher

-- | represent a protocol registered on the 'WarpDispatcher'
type WarpMode = Mode (WarpState -> DspOutput WProtocol WarpMsg)

-- | helper to buid 'WarpMode', when not needing the WarpState
makeWarpMode :: WProtocol -> Mode (WarpState -> PipeEnd WarpMsg) -> WarpMode
makeWarpMode i m = run <$> parseModeParameter m (parseProtocol reader i)
    where 
        run (f,id) ws = (f ws,id)
        reader = WProtocol <$> maybeReader readMaybe

-- | helper to buid 'WarpMode', when not needing the WarpState
simpleWarpMode :: WProtocol -> Mode (PipeEnd WarpMsg) -> WarpMode
simpleWarpMode i m = (\m _ -> m) <$> dspOutput reader i m
    where reader = WProtocol <$> maybeReader readMaybe

-- | Generate the LinkMode "warp" from a default 'Warp' configuration, and a list of 'WarpMode'.
--
-- each 'WarpMode' correspond to a possible protocol on warp: the user choses at runtime which are used.
runWarpModes :: DefaultWarp -> [WarpMode] -> LinkMode
runWarpModes w@(DefaultWarp linkPID _ _ _ ) wmds = dspOutput auto linkPID $ runWarp <$> manyModes wmds mode
    where
        mode = Mode "warp" "Wow, another routing protocol" noRootCalls $ parseWarp w :: Mode Warp
        runWarp :: (Warp, [WarpState -> DspOutput WProtocol WarpMsg]) -> PipeEnd Payload
        runWarp (warp, outputs) c = do
            ws <- makeWarp warp 
            pure c >>= decoder >>= pipe ws >>= runDispatcher warpDispatcher (map ($ws) outputs)

-- | Parse a 'Warp' configuration from the options.
--
-- the argument is the default timeout.
parseWarp :: DefaultWarp -> Parser Warp
parseWarp (DefaultWarp _ widM ttl timeout) = Warp 
    <$> (WID <$> option auto (long "id" <> metavar "USER_ID" <> maybeDefaultWID <> help "the Warp user identifier used"))
    <*> (TTL <$> option auto (long "max-ttl" <> metavar "TTL"  <> value ttl <> showDefault <> help "maximum number of hop for search packets"))
    <*> option auto (long "timeout" <> metavar "SECONDS"  <> value timeout <> showDefault <> help "maximum time of inactivity before an edge is removed from the network graph")
    <*> many (RID <$> option auto (long "ressources" <> short 'r' <> metavar "RID" <> help "ressources offered by this user" ))
        where maybeDefaultWID = case widM of
                    Nothing -> mempty
                    Just wid -> value wid

-- | Relevant default parameters for 'Warp' 
data DefaultWarp = DefaultWarp {
    warpLinkPID :: Int,               -- ^ protocol ID used on the link layer
    warpID :: Maybe Int,
    warpMaxTTL :: Int,
    warpTimeout :: Int                -- ^ edge timeout in 'Warp'
}


