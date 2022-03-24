{-# LANGUAGE TemplateHaskell #-}
module Warp.NAT.Leases (
    NATState,
    LeaseID (..), 
    makeNATState,
    allocateLease, 
    freeLease, 
    refreshLease,
    hasLease,
    isLeased,
    getLeases,
) where


import Control.Monad.IO.Class
import Prelude hiding(error)
import qualified Data.Map as M
import Control.Concurrent.Timer
import Control.Concurrent.Suspend

import ADN
import Data.IP as IP
import Data.IP.Header
import Warp.Types 
import Warp.NAT.Packet


data NATState = NATState {
    timeout :: Delay,
    natWIDMap :: ADNVar NATMap,
    natIPPool :: ADNVar NATPool
}

type NATPool = IPPool Lease
type NATMap = M.Map WID IP

data Lease = Lease {leaseWID :: WID,
                    timer :: TimerIO
                    }

-- | identifier for a Lease: can be a Warp UserID, or a IP in the NAT
data LeaseID = LeaseWID WID | LeaseIP IP
    deriving Show

makeNATState :: Delay -> IP -> IP -> ADN NATState
makeNATState timeout ipStart ipEnd = do
    mV <- newADNVar M.empty
    pV <- newADNVar p
    pure $ NATState timeout mV pV
        where p = emptyIPPool ipStart ipEnd

allocateLease :: NATState -> WID -> ADN (Maybe IP)
allocateLease nat wid = modifyLeases nat insert
    where insert :: NATMap -> NATPool -> ADN (NATMap, NATPool, Maybe IP)
          insert m p = do
            liftADN <- makeLift
            timer <- liftIO $ newTimer
            let lease = Lease wid timer
                (natIP, p') = allocIP p lease 
            startTimer <- liftIO $ oneShotStart timer (liftADN $ freeLease nat $ LeaseIP natIP) $ timeout nat
            if startTimer then pure (M.insert wid natIP m, p', Just natIP)
            else error ("failed to start lease timeout... aborting allocation for: " ++ show wid) >> pure (m,p, Nothing)
        
freeLease :: NATState -> LeaseID -> ADN ()
freeLease nat lID = case lID of
        LeaseWID wid -> modifyLeases_ nat $ freeWID wid
        LeaseIP ip -> modifyLeases_ nat $ free ip
    where
        freeWID wid m p = case M.lookup wid m of Nothing -> warning ("attempted to free unkown userID : " ++ show wid) >> pure (m,p)
                                                 Just ip -> free ip m p
        free :: IP -> NATMap -> NATPool -> ADN (NATMap,NATPool)
        free ip m p = case lookupIP p ip of
            Nothing -> warning ("attempted to free unkown IP lease: " ++ show ip) >> pure (m,p)
            Just l -> killLease l >> pure (M.delete (leaseWID l) m, freeIP p ip)
        killLease :: Lease -> ADN ()
        killLease l = liftIO $ stopTimer (timer l)

refreshLease :: NATState -> LeaseID -> ADN Bool
refreshLease nat lID = lookupLease nat lID >>= refresh
    where refresh lM = case lM of
            Nothing -> warning ("attemptend to refresh a non-existing lease: " ++ show lID ) >> pure False
            Just l -> liftIO $ oneShotRestart (timer l)

hasLease :: NATState -> WID -> ADN (Maybe IP)
hasLease nat wid = readADNVar (natWIDMap nat) >>= look
    where look m = case M.lookup wid m of
                        Nothing -> debug ("unable to find a lease for user: " ++ show wid) >> pure Nothing
                        Just ip -> pure $ Just ip

isLeased :: NATState -> IP -> ADN (Maybe WID)
isLeased nat ip = fmap leaseWID <$> lookupLease nat (LeaseIP ip)

getLeases :: NATState -> ADN [(WID,IP)]
getLeases nat = M.assocs <$> readADNVar (natWIDMap nat)

-- ## INTERNAL ##

lookupLease :: NATState -> LeaseID -> ADN (Maybe Lease)
lookupLease nat (LeaseWID wid) = hasLease nat wid >>= look
        where look = maybe (pure Nothing)  (lookupLease nat . LeaseIP) 

lookupLease (NATState _ _ poolV) (LeaseIP ip) = look <$> readADNVar poolV 
        where look p = lookupIP p ip

modifyLeases_ :: NATState -> ( NATMap -> NATPool -> ADN (NATMap, NATPool) ) -> ADN ()
modifyLeases_ (NATState _ mapV poolV) f = modifyADNVar_ poolV $ \p -> modifyADNVar mapV (\m -> f m p)

modifyLeases :: NATState -> ( NATMap -> NATPool -> ADN (NATMap, NATPool, a) ) -> ADN a
modifyLeases (NATState _ mapV poolV) f = modifyADNVar poolV $ \p -> modifyADNVar mapV (\m -> toTuple <$> f m p)
    where toTuple (m',p',a) = (m',(p',a))



