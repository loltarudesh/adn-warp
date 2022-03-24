{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ADN.Debug.Buffer (
    Buffer(..) 
  ) where

import Control.Monad.IO.Class
import qualified Control.Concurrent.Chan.Unagi.Bounded as UBC

import ADN.Core


-- | A Buffer is a simple Pipe, which buffers packets between two layers.
-- For debugging purposes: a 'Warning' log is sent if the buffer is full (allows to track down bottleneck).
data Buffer p = Buffer {
    bufferName :: String,
    bufferSize :: Int}

-- | buffers packet between, and send 'Warning' when the buffer is full
instance PipeBuilder (Buffer p) p p where
    pipe (Buffer name size) (Channel si so) = block name $ do
        (i1,o1) <- liftIO $ UBC.newChan size
        (i2,o2) <- liftIO $ UBC.newChan size
        liftADN <- makeLift
        let read o = liftIO $ UBC.readChan o 
            write dirB i p = liftADN $ do
                let dir = if dirB then "incoming" else "outgoing"
                b <- liftIO $ UBC.tryWriteChan i p 
                if b then pure ()
                else warning (dir ++ " buffer is full") >> liftIO (UBC.writeChan i p)
        linkSinkToSource (write True i1) so
        linkSinkToSource si (read o2)
        return $ Channel (write False i2) (read o1)
        



