-- | Production of 'ADNMode' with 'Warp'
--
-- the whole stack includes 2 'Dispatcher': 
--
--  * the /link dispatcher/, between 'LinkUDP'and 'Warp'
--
--  * the /warp dispatcher/, between 'Warp' and the applications ('NATServer' for example)
--
--
-- In order to produce a 'ADNMode', required by 'runADNModes', you must :
--
-- * create the 'WarpMode' with 'natClient' and 'natServer' (and any other 'WarpMode')
--
-- * use 'WarpMode' to produce a 'LinkMode' from multiple 'WarpMode'. Needs the flag "--warp".
--
-- * use 'linkMode' to produce a single ADN-Mode, associated with the command "link-UDP", from multiples 'LinkMode'
-- 
-- NOTE: the use of 'DispatchMode' allows to include all the /possible/ modes: the user will chose at runtime the modes loaded, with options in the arguments.
-- Each mode using a different protocol ID, there should be no conflict between them. 
--
module Warp.Modes (
    -- * link dispatcher
    LinkMode,
    runLinkModes,

    -- * Warp modes
    WarpMode,
    runWarpModes,
    makeWarpMode,
    simpleWarpMode,

    -- * NAT modes
    natClient,
    natServer,

    -- * Parsing arguments
    parseLink, 
    parseWarp,

    -- * Default structure
    DefaultLink(..),
    DefaultWarp(..),
    DefaultClient(..), 
    DefaultServer(..), 
    )where

import Warp.Modes.Link
import Warp.Modes.Warp
import Warp.Modes.NAT


