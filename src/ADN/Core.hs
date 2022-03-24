-- |This is the core of the ADN framework: a set of tools designed to implement easily ADN modules, and connect them together.
module ADN.Core (
    -- |the ADN monad: basically IO, with closing, logging, and order handling
    module ADN.Core.Block,
    -- | Global static configuration for the ADN monad
    module ADN.Core.Conf,
    -- |structure of the pipeline: connecting module working on bidirectionnal flows of packets.
    module ADN.Core.Pipe,
    -- |manipulation of raw 'Data.ByteString.ByteString' packets
    module ADN.Core.Packets,
    -- |defining priviledged action, to be run outside the main program
    module ADN.Core.RootAccess,
    -- |simple wrapper around "Control.Concurrent.MVar" to allow the use of ADN action inside callbacks.
    module ADN.Core.ADNVar
    )where

import ADN.Core.Conf
import ADN.Core.Block
import ADN.Core.Packets
import ADN.Core.Pipe
import ADN.Core.ADNVar
import ADN.Core.RootAccess

