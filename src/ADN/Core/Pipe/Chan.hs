{- | This is an internal module, which only function is to chose which implementation of Chan to use.
 - You should not import this module, use wrapper ADN.Core.Pipe.Tools connectedSinkSource instead
 -}
module ADN.Core.Pipe.Chan (
    makeChan
 )
where

import Control.Monad.IO.Class
import qualified Control.Concurrent.Chan as BC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Control.Concurrent.Chan.Unagi.Bounded as UBC

import ADN.Core.Conf
import ADN.Core.Block
import ADN.Core.Pipe.Class



makeChan :: ADN ( Sink a, Source a )
makeChan = do
    chanImplem <- adnChanImplem <$> readADNConf
    (si,so) <- liftIO $ makeChan_ chanImplem
    return (liftIO . si, liftIO so)
    

makeChan_ :: ChanImplem -> IO (a -> IO (), IO a)
makeChan_ BaseChan = do
        c <- BC.newChan
        return $ (BC.writeChan c, BC.readChan c)

makeChan_ TQueue = do
    q <- TQ.newTQueueIO
    let si = STM.atomically . TQ.writeTQueue q
        so = STM.atomically $ TQ.readTQueue q
    return (si,so)

makeChan_ (TBQueue b) = do
    q <- TBQ.newTBQueueIO $ toEnum b
    let si = STM.atomically . TBQ.writeTBQueue q
        so = STM.atomically $ TBQ.readTBQueue q
    return (si,so)

makeChan_ (UnagiBChan b) = do
        (i,o) <- UBC.newChan b
        return $ (UBC.writeChan i, UBC.readChan o)

