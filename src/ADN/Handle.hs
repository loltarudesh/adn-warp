{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ADN.Handle( ADNHandle(..) ) where

import Control.Monad.IO.Class
import ADN.Core
import GHC.IO.Handle
import Data.ByteString as BS


-- | A simple wrapper around standard "GHC.IO.Handle".
-- the MTU has to be specified at the creation.
data ADNHandle = ADNHandle {
    mtu :: Int,
    handle :: Handle
}


instance PipeStartBuilder ADNHandle Payload where
    pipeStart (ADNHandle mtu handle) = do
        registerCloseAfter $ do
            debug $ "closing Handle"
            liftIO $ hClose handle
        return $ Channel sendD recvD
            where 
                sendD (Payload p) = do
                    liftIO $ hPut handle p >> hFlush handle
                    debug $ "sent \t" ++ show (BS.length p) ++ " bytes"
                recvD = do
                    p <- liftIO $ hGetSome handle mtu
                    debug $ "received\t" ++ show (BS.length p) ++ " bytes" 
                    return $ Payload p


instance PipeEndBuilder ADNHandle Payload where
    pipeEnd adnH c1 = do
        c2 <- pipeStart adnH
        linkChannels c1 c2
        


