module ADN.Core.ADNVar where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import ADN.Core.Block

-- | A simple wrapper around "Control.Concurrent.MVar" for 'ADN' monad
newtype ADNVar a = ADNVar (MVar a)

newADNVar :: a -> ADN (ADNVar a)
newADNVar a = liftIO $ ADNVar <$> newMVar a

readADNVar :: ADNVar a -> ADN a
readADNVar (ADNVar v) = liftIO $ readMVar v

swapADNVar :: ADNVar a -> a -> ADN a
swapADNVar (ADNVar v) a = liftIO $ swapMVar v a

withADNVar :: ADNVar a -> (a -> ADN b) -> ADN b
withADNVar (ADNVar v) f = do
    lift <- makeLift
    liftIO $ withMVar v (lift . f)


modifyADNVar_ :: ADNVar a -> (a -> ADN a) -> ADN ()
modifyADNVar_ (ADNVar v) f = do
    lift <- makeLift
    liftIO $ modifyMVar_ v (lift . f)

modifyADNVar :: ADNVar a -> (a -> ADN (a, b)) -> ADN b
modifyADNVar (ADNVar v) f = do
    lift <- makeLift
    liftIO $ modifyMVar v (lift . f)



