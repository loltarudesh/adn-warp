-- | ADN is based on the construction of pipelines.
--
-- a pipeline is a chain of modules processing packets. By convention, 
-- the start of the pipeline is "toward the network", and its end is "toward to applicative layer".
-- 
-- a simple pipeline is build with bind operation:
--
-- > myPipeStart >>= myPipe >>= myPipeEnd
--
-- Each link (bind) represent a bidirectionnal flow of packet. The modules communicates with one another
-- by exchanging 'Channel', very similar in use to a regular network sockets.
module ADN.Core.Pipe (
    -- * PipeStart
    PipeStart(..), PipeStartBuilder(..),
    pipeEndToStart, 

    -- * Pipe
    Pipe(..), PipeBuilder(..),
    OnLowPkt(..), OnUpPkt(..),
    makePipe, makePipeState,
    makeSimplePipe, makeSimplePipeState,

    -- * PipeEnd
    PipeEnd(..), PipeEndBuilder(..),
    pipeStartToEnd,

    -- * Channels
    Sink(..), Source(..), Channel(..),
    linkChannels,
    connectedChannels, 
    linkSinkToSource, 
    connectedSinkSource,
    ) where 

import ADN.Core.Pipe.Class
import ADN.Core.Pipe.Tools


