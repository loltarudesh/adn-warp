{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A 'Dispatcher' splits a pipeline into multiples one
--
-- It is a 'PipeEnd', consuming input from a lower layer. Each output is represented by a 'Protocol', associated with an ID exctracted from the packet.
-- 
-- A 'Protocol' can then be used as a 'PipeStart' to build the pipeline for this protocol ID.
--
-- to implement a new protocol for an existing 'Dispatcher', you should probably create a 'DispatchMode', and insert it in the list called by 'dispatchMode'.
--
-- this will allow the user to chose activate and configure your protocol at runtime 
module ADN.Dispatcher (
    -- * Dispatcher
    Dispatcher,
    Protocol(..),
    -- * building dispatchers
    simpleDispatcher,byteDispatcher,makeDispatcher,

    -- * Using dispatcher
    DspOutput(..),
    dspOutput,
    runDispatcher,

    -- * Parsing arguments
    parseProtocol,
  ) where

import Prelude hiding(error)
import Options.Applicative 
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Map as M

import ADN.Core
import ADN.Modes


-- | represent an output of a dispatcher
--data DspOutput i p = DspOutput i (PipeEnd p)
type DspOutput i p = (PipeEnd p, i)

-- | build a DspOutput mode from a default protocol ID, and a mode consuming the output
-- Parse the option "--protocol-id" from the arguments, with a default value in arguments
dspOutput :: (Eq i, Ord i, Show i)  => ReadM i -> i -> Mode (PipeEnd p) -> Mode (DspOutput i p)
dspOutput reader def m = parseModeParameter m $ parseProtocol reader def -- Mode n d g $ DspOutput <$> parseProtocol reader def <*> end

parseProtocol :: Show i => ReadM i -> i -> Parser i
parseProtocol reader def = option reader (long "protocol-id" <> metavar "PROTOCOL_ID" <> value def <> showDefault <> help "protocol number used")

-- | run a given dispatcher, and register the list of output provided
runDispatcher :: (Eq i, Ord i, Show i) => ADN (Dispatcher i p p') -> [DspOutput i p'] -> PipeEnd p
runDispatcher mkDsp ends c = do
    dsp <- mkDsp
    pure c >>= pipeEnd dsp
    forM_ ends $ genEnd dsp
        where genEnd dsp (end, pid) = pipeStart (Protocol dsp pid) >>= end


-- | A Dispatcher is a module designed to split a pipeline into multiple ones.
-- It takes packet p from the lower layer, and dispatch packet p' to multiple upper layers, each identified by a key i. 
data Dispatcher i p p' = Dispatcher {
    protocolMap :: ADNVar (M.Map i (Sink p')),
    downSink :: Sink p,
    downSource :: Source p,
    readProtocolNumber :: DispatchLow i p p',
    writeProtocolNumber :: DispatchUp i p p'
}

-- | function called on packet from the lower layer. Return a DispatchAnswer, which can notably be a broadcast or multicast.
type DispatchLow i p p' = p -> ADN ([DispatchAnswer i p'])
-- | return type of DispatchLow: can be either a Broadcast packet, or a directionnal one.
data DispatchAnswer i p = Broadcast p | SendTo i p

-- | function called on packet from the upper layer. Return the list of packets to be sent to the lower layer.
type DispatchUp i p p' = i -> p' -> ADN [p]


-- | A simple dispatcher, transmitting packet one-to-one, with pure function for reading identifier, and appending it.
simpleDispatcher :: (p -> (i,p')) -> (i -> p' -> p) -> ADN (Dispatcher i p p')
simpleDispatcher r w = makeDispatcher readPkt writePkt
        where 
            writePkt i p' = pure [w i p']
            readPkt p = pure [SendTo dest p']
                where (dest,p') = r p

-- | The simplest of dispatcher: reads the byte (Word8) of a packet coming from the lower layer as the identifier, and send the reamining payload to 
-- the corresponding protocol.
byteDispatcher :: ADN (Dispatcher Int Payload Payload)
byteDispatcher = simpleDispatcher readWord8 writeWord8


-- | builds a dispatcher from DispatchLow and DispatchUp functions.
makeDispatcher :: DispatchLow i p p' -> DispatchUp i p p' -> ADN (Dispatcher i p p')
makeDispatcher readPkt writePkt = do
    (si,so) <- connectedSinkSource
    m <- newADNVar M.empty
    return $ Dispatcher m si so readPkt writePkt


-- | A Dispatcher act as a PipeEnd: it simply consumes packet from the lower layer.
-- the multiples upper layers are each represented by a 'Protocol'
instance (Ord i, Show i) => PipeEndBuilder (Dispatcher i p p') p where
    pipeEnd l c = do
        debug "starting dispatcher"
        linkSinkToSource (send c) (downSource l) 
        linkSinkToSource (onPacket l) (recv c)
            where
                callSend p f = f p
                onPacket l p = do
                    m <- readADNVar $ protocolMap l
                    ans <- readProtocolNumber l p
                    forM_ ans $ \a -> case a of
                        Broadcast p' -> mapM_ (callSend p') $ M.elems m
                        SendTo i p' -> maybe onError (callSend p') $ M.lookup i m 
                                where onError = debug $ "packet received for unknown protocol: " ++ show i


-- | A Protocol represent one output of a 'Dispatcher', associated with a Protocol ID.
data Protocol i p p' = Protocol {
    protocolDispatcher :: Dispatcher i p p',
    protocolID :: i }

-- | A Protocol is as a PipeStart, producing only packet adressed to this identifier.
instance (Ord i, Show i) => PipeStartBuilder (Protocol i p p') p' where
    pipeStart (Protocol dsp id) = do
        debug $ "starting new protocol: " ++ show id
        (si,so) <- connectedSinkSource
        modifyADNVar_ (protocolMap dsp) $ insertProtocol id si 
        let chanSend x = do
                ps <- writeProtocolNumber dsp id x
                forM_ ps $ downSink dsp 
        return $ Channel chanSend so
            where
                insertProtocol id si m = case M.lookup id m of
                    Just _ -> do
                        error $ "Protocol ID " ++ show id ++ " is already in use"
                        return m
                    Nothing -> pure $ M.insert id si m




