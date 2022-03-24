module ADN.Core.Pipe.Tools (
    OnLowPkt(..), OnUpPkt(..),
  
    linkSinkToSource, linkChannels,
    connectedChannels, connectedSinkSource,
    pipeEndToStart, pipeStartToEnd,

    makePipe, makePipeState,
    makeSimplePipe, makeSimplePipeState,
  ) where 


import Control.Monad

import ADN.Core.Block
import ADN.Core.Pipe.Class
import ADN.Core.Pipe.Chan

-- |function called on packet coming from the lower layer
type OnLowPkt p p' = Sink p  -- ^handle to send packet to the lower layer
                   -> Sink p' -- ^handle to send packet to the upper layer
                   -> p       -- ^incoming packet from the lower layer
                   -> ADN ()  -- ^processing the packet
-- |function called on packet coming from the upper layer
type OnUpPkt p p'  = Sink p  -- ^handle to send packet to the lower layer
                   -> Sink p' -- ^handle to send packet to the upper layer
                   -> p'      -- ^incoming packet from the upper layer
                   -> ADN ()  -- ^processing the packet

-- | Helper to generate PipeBuilder instances from 2 functions, called on packet incoming from the upper and the lower layers.
-- 
-- This function needs to create 2 chans and 2 threads: it is intended for relatively heavy and intricate operation.
-- If you are writing a very simple pipe, you should try using 'makeSimplePipe', or you're own instance of 'Pipe'.
--
makePipe :: OnLowPkt p p' -> OnUpPkt p p' -> Pipe p p'
makePipe onLow onUp (Channel toLow genLow) = do
    (toUp, genUp) <- connectedSinkSource
    let fromUp = onUp toLow toUp 
        fromLow = onLow toLow toUp 
    linkSinkToSource fromLow genLow
    return $ Channel fromUp genUp 

-- | Similar to makePipe, but take a state builder as a first parameter, and gives the result to the two callback function.
makePipeState :: ADN s -> (s -> OnLowPkt p p') -> (s -> OnUpPkt p p') -> Pipe p p'
makePipeState mk l u c = mk >>= \st -> makePipe (l st) (u st) c


-- | A pure implementation of a PipeBuilder instance, suitable for simple pipes.
-- Less powerful than 'makePipe', but no thread or chan are created.
makeSimplePipe :: (p -> ADN (Maybe p')) -> (p' -> ADN (Maybe p)) -> Pipe p p'
makeSimplePipe onLow onUp (Channel toLow genLow) = pure $ Channel fromUp genUp
    where
        fromUp p' = onUp p' >>= maybe (pure ()) toLow 
        genUp = genLow >>= onLow >>= maybe genUp pure

-- | Similar to makeSimplePipe, but take a state builder as a first parameter, and gives the result to the two callback function.
makeSimplePipeState :: ADN s -> (s -> p -> ADN (Maybe p')) -> (s -> p' -> ADN (Maybe p)) -> Pipe p p'
makeSimplePipeState mk l u c = mk >>= \st -> makeSimplePipe (l st) (u st) c

-- | Convert a PipeStartBuilder in PipeEnd
pipeStartToEnd :: PipeStartBuilder a p => a -> PipeEnd p
pipeStartToEnd ps c = pipeStart ps >>= linkChannels c

-- | Convert a PipeEndBuilder in PipeStart
pipeEndToStart :: PipeEndBuilder a p => a -> PipeStart p
pipeEndToStart pe = do (c1,c2) <- connectedChannels
                       pipeEnd pe $ c1
                       pure c2


-- | Links 2 Channel together:
--  take every packet coming from one, and send it to the other (and vice-versa)
--  WARNING: this function create 2 threads
linkChannels :: Channel p -> Channel p -> ADN ()
linkChannels (Channel si1 so1) (Channel si2 so2) = do
    linkSinkToSource si1 so2
    linkSinkToSource si2 so1

-- | Generate a pair of connected Channels :
--  every packet sent to one Channel arrives in the other (and vice-versa)
--  WARNING: this function create 2 connectedSinkSource (represented by STM TQueue)
--           This means that packets are buffered betweens the sockets, and can pile-up if not read fast enough.
connectedChannels :: ADN (Channel p, Channel p)
connectedChannels = do
    (si1,so1) <- connectedSinkSource
    (si2,so2) <- connectedSinkSource
    let c1 = Channel si1 so2
        c2 = Channel si2 so1
    return (c1,c2)


-- | Link a sink to a source: reads from the source, and sends the packet to the sink
--  WARNING: thread created.
linkSinkToSource ::  Sink p -> Source p -> ADN ()
linkSinkToSource si so = forkADN . forever $ so >>= si

-- | Create a pair of connected (Sink, Source): a very simple wrapper around STM.TQueue
--  WARNING: packets are buffered, and can pile-up if the Sink does not handle them fast enough.
connectedSinkSource::  ADN (Sink p, Source p)
connectedSinkSource = makeChan
            

{-

    -- this is fun... But maybe it's for the best if no one ever runs it!
function666 :: ( Sink -> Sink ) -> IO (Source -> Source)
function666 callback source = do
    c <- newTChan
    let onLow = callback $ writeChan c :: Sink
        res :: Source -> Source
        res fromLow = getMaybePacket >>= maybe (fromLow >>= onLow >> res) pure
        getMaybePacket :: IO (Maybe Packet)
        getMaybePacket = atomically tryReadTChan 
    return res

-}

