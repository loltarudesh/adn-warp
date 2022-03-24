-- |This module defines the execution monad ADN. It is basically IO, with some useful functionnalities, in particular:
--
--  * Definition of 'Block', allowing for independent treatement
--
--  * Handling of closing callback
--
--  * Logging with source 'Block' and level
--
--  * Basic orders system
--
module ADN.Core.Block (
    -- *the ADN monad
    ADN, 
    runADN,
    forkADN,
    makeLift,

    -- * configuration for ADN
    ADNConf(..),
    readADNConf,

    -- *Blocks
    Block, 
    BlockName(..), 
    block,
    forkBlock,
    maybeInSubBlock,

    -- *closing
    registerClose,
    registerCloseAfter,
    closeThisBlock,
    closeSubBlock,


    -- * Logging functions
    LogLevel(..), 
    Log(..), 
    debug, info, warning, error,
    sendLog,
    registerLogger,
 

    -- *orders
    CLI(..),
    addOrderCLI, 
    addOrderCLI_, 
    runBlockCLI, 
    getOrders,

    -- *misc
    ThreadID
    )where

import Prelude hiding (error)
import ADN.Core.Block.Run 
import ADN.Core.Block.ADN 
import ADN.Core.Block.Tools 
import ADN.Core.Block.Close 
import ADN.Core.Block.Log 
import ADN.Core.Block.CLI
import ADN.Core.Conf

