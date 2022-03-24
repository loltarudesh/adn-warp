{-# LANGUAGE MultiParamTypeClasses #-}
-- | A NAT client will route default internet traffic into the ADN network, to a server (acting as a gateway).
--
-- it requires the creation of a TUN interface, as well as a runtime elevated program
-- allowing it to set the interface's IP during execution.
module Warp.NAT.Client (
  NATClient(..), 
  ) where

import Prelude hiding(error)
import Text.Read
import Control.Monad.IO.Class
import Control.Concurrent.Timer
import Control.Concurrent.Suspend

import ADN
import Data.IP
import Data.IP.Header
import Data.IP.Packet
import Warp.Types
import Warp.Link.Tun
import Warp.NAT.Packet
import Warp.Utils



-- | Configuration structure for a NAT client
data NATClient = NATClient {
    natclScriptName :: String,      -- ^ name of the runtime elevated program
    natclIface :: ADNTun,            -- ^ TUN interface configuration
    natclTimeout :: Delay
}


data Server = Server {
    srvWID :: WID,
    leaseIP :: IP,
    srvTimer :: TimerIO
}

type ServerVar = ADNVar (Maybe Server)

data State = State {
    natclConf :: NATClient,
    natclServer :: ServerVar,
    toWarp :: Sink NATPkt
}

-- | create a TUN interface, and a runtime program to set the interface's IP
instance RootAccess NATClient where 
    root (NATClient scriptName tun _) = [RootRuntime desc scriptName [("SCRIPT_DESCRIPTION", desc), ("ADN_IFACE_NAME", tunName tun)], RootRequestTun (tunName tun)]
        where desc = "Allowing the module NAT Client to change the IP address and the MTU of the ADN virtual interface "++ tunName tun

-- | route IP traffic over ADN, to a 'NATServer'.
instance PipeBuilder NATClient NATPkt IPPkt where
    pipe conf c = block "client" $ makeSimplePipeState make onNatPkt onIPPkt c
        where make = makeState conf $ send c


-- | Function called on outgoing packet, coming from the TUN interface
onIPPkt :: State -> IPPkt -> ADN (Maybe NATPkt)  
onIPPkt (State conf sM _) p = readADNVar sM >>= maybe noServer (sendToServer p) -- if we have a lease with a server, send it the packet
    where 
        noServer = warning "attempted to send IP data, but not connected to a server" >> pure Nothing   
        sendToServer ipPkt (Server sID sIP _) = if sIP == src then do
                debug $ "sending ipPkt to source " ++ show sID ++ " ("++show src++") -> ("++show dst++")" 
                return . Just $ NATCom sID ipPkt 
                else pure Nothing
            where src = iph_sourceIP $ ip_header ipPkt                                                      
                  dst = iph_destIP $ ip_header ipPkt


-- | Function called on incoming packet, coming from the network through Warp
onNatPkt :: State -> NATPkt -> ADN (Maybe IPPkt)
onNatPkt st (NATCom wid ipPkt) = onNATCom st wid ipPkt 
onNatPkt st (OfferLease uid ip) = startServer st uid ip >> return Nothing
onNatPkt st (NATErr uid e) = onNATError st uid e >> return Nothing
onNatPkt _ _ = warning "received invalid packet type" >> return Nothing




onNATCom :: State -> WID -> IPPkt -> ADN (Maybe IPPkt)
onNATCom state@(State conf st _) wid ipPkt = readADNVar st >>= maybe noServer isServer
    where 
        noServer = do
            warning $ "received NATCom packet from " ++ show wid ++ ", but no server registered..."
            pure Nothing
        isServer (Server sid sip _) = if wid == sid && dst == sip then do
                debug $ "client received ipPkt from " ++ show wid ++ " (dest IP : "++show dst ++ ")"
                refreshServer state
                pure $ Just ipPkt
            else do
                warning $ "received NATCom packet with mismatch lease, from " ++ show wid ++ " (dest IP : " ++ show sip ++ ")"
                pure Nothing

        dst = iph_destIP $ ip_header ipPkt

onNATError :: State -> WID -> NATError -> ADN ()
onNATError state@(State conf st _) uid e = modifyADNVar_ st $ maybe (pure Nothing) onError
    where 
        onError :: Server -> ADN (Maybe Server)
        onError s@(Server sID sIP _) =
           let err m = if sID == uid then isServer s m else pure (Just s)
           in case e of LeaseFailed -> err "failed"
                        LeaseUnknown -> err "unknown"
                        LeaseTaken -> err "taken"
        isServer s m = do 
            warning ("received NAT error from source: lease " ++ m)
            closeServer state s
            return Nothing


    
makeState :: NATClient -> Sink NATPkt -> ADN State
makeState conf toLow = do
    st <- State conf <$> newADNVar Nothing <*> pure toLow
    registerClientCLI st
    return st



registerClientCLI :: State -> ADN ()
registerClientCLI state@(State conf st toWarp) = do
    addOrderCLI "request" "request UID : request a lease from the the NAT server UID" sendReq
    addOrderCLI "addServer" "addSource WID IP : manually register a server" addServer
    addOrderCLI_ "free" "free the current lease" freeLease
    addOrderCLI_ "show" "display the current server (if any)" showLease
        where
            sendReq a = case readMaybe a of
                            Nothing -> pure "unable to read arguments"
                            Just uid -> do toWarp $ RequestLease (WID uid)
                                           return $ "lease request sent to " ++ show uid 
            addServer a = case parseArgs of
                            Nothing -> pure "unable to read arguments"
                            Just (wid,ip) -> startServer state wid ip >> pure ("registered server " ++ show wid ++ " with lease " ++ show ip)
                where 
                    parseArgs :: Maybe (WID, IP)
                    parseArgs = if length args == 2 then (,) <$> (WID <$> readMaybe (args !! 0) :: Maybe WID) <*> pure (readAddr (args !! 1 ))  -- TODO crashes if IP parse fail
                                                     else Nothing
                    args = words a :: [String]

            freeLease = do
                sM <- stopServer state
                case sM of
                    Nothing -> pure "no source to free"
                    Just wid -> pure $ "removed source " ++ show wid

            showLease = do
                sM <- readADNVar st
                case sM of
                    Nothing -> pure "not connected to a server"
                    Just (Server wid ip _) -> pure $ "currently connected to server: " ++ show wid ++ " (leased IP : " ++ show ip ++ ")"


stopServer :: State -> ADN (Maybe WID)
stopServer state@(State conf st toWarp) = modifyADNVar st stopMaybe
    where 
        stopMaybe sM = maybe (pure (Nothing,Nothing)) closeSrv sM
        closeSrv s = do
            wid <- closeServer state s
            toWarp $ FreeLease wid
            return $ (Nothing, Just wid)
            

closeServer :: State -> Server -> ADN WID
closeServer (State conf st _) (Server wid _ timer) = do
    debug $ "closing link with server " ++ show wid
    stopNATClient conf
    liftIO $ stopTimer timer
    return wid
        where 
            stopNATClient :: NATClient -> ADN ()
            stopNATClient (NATClient scriptName _ _) = callRuntime scriptName ["delete"]


startServer :: State -> WID -> IP -> ADN ()
startServer state@(State conf st _) wid ip = do
    liftADN <- makeLift
    timer <- liftIO $ newTimer
    let server = Server wid ip timer
    startTimer <- liftIO $ oneShotStart timer (liftADN serverTimeout) $ natclTimeout conf
    if startTimer then do
        info $ "Registering source: " ++ show wid ++ " -> " ++ show ip
        oldSrv <- swapADNVar st $ Just server
        case oldSrv of 
            Nothing -> pure ()
            Just s -> closeServer state s >> pure ()
        setNATClient conf ip
    else error $ "failed to start timer... aborting server start for: " ++ show wid
        where 
            serverTimeout = do
                debug $ "connexion to server timeout, closing link..."
                stopServer state
                pure ()
            setNATClient :: NATClient -> IP -> ADN ()
            setNATClient (NATClient scriptName tun _) ip = callRuntime scriptName ["set", show ip, show $ tunMTU tun]


refreshServer :: State -> ADN Bool
refreshServer (State _ st _) = readADNVar st >>= maybe onError refresh
    where
        refresh (Server _ _ timer) = liftIO $ oneShotRestart timer
        onError = warning "Attempted to refresh, but no server registered..." >> pure False



