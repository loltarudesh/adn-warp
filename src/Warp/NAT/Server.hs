{-# LANGUAGE MultiParamTypeClasses #-}
-- | a NAT server share its internet access with clients, over ADN
--
-- it requires the creation of a TUN interface, as well as setting up 
-- a NAT, routing the traffic incoming from the client to a gateway interface (with internet access!).
--
-- WARNING: no check are made about the validity of the parameters!!
-- 
-- the user has to ensure that the range of allocated IP are in the same network as the server's IP.
--
-- the network mask is for now fixed to /24 (255.255.255.0).
module Warp.NAT.Server (
  -- * NATServer configuration and service
  NATServer(..),
  ) where

import Control.Concurrent.Suspend
import Control.Monad
import Text.Read

import ADN
import Data.IP
import Data.IP.Header
import Data.IP.Packet
import Warp.Types
import Warp.Utils
import Warp.NAT.Packet
import Warp.NAT.Leases
import Warp.Link.Tun


-- | Configuration structure for a NAT server
data NATServer = NATServer {
                natsIface :: ADNTun,    -- ^ TUN interface configuration
                inetIface :: String,    -- ^ name of the gateway interface
                serverIP :: IP,         -- ^ IP of the server in the NAT
                dhcpRange :: (IP,IP),   -- ^ range of IPs allocated to clients
                leaseTimeout :: Delay   -- ^ duration validity of a lease
                }


-- | Share an internet access over ADN with a 'NATClient'
instance PipeBuilder NATServer NATPkt IPPkt where
    pipe conf = block "server" . makePipeState makeState onWarpPkt onTunPkt 
        where 
            makeState = do
                st <- makeNATState timeout ipStart ipEnd
                registerServerCLI st
                pure st
            (ipStart,ipEnd) = dhcpRange conf
            timeout = leaseTimeout conf

-- | Create a TUN interface, and setup a NAT 
instance RootAccess NATServer where
    root nat = [RootBefore "Configuring the NAT gateway" calls, RootRequestTun adn_iface]
        where
            calls = ["sysctl net.ipv4.ip_forward=1",
                     "iptables -t nat -A POSTROUTING -o " ++ inet_iface ++ " -j MASQUERADE",
                     "ip link set " ++ adn_iface ++ " down",
                     "ip link set " ++ adn_iface ++ " mtu " ++ mtu,
                     "ip addr flush " ++ adn_iface,
                     "ip addr add " ++ adn_ip ++ "/24 dev " ++ adn_iface,
                     "ip link set " ++ adn_iface ++ " up"]
            adn_iface = tunName tun 
            inet_iface = inetIface nat
            adn_ip = show $ serverIP nat
            mtu = show $ tunMTU tun 
            tun = natsIface nat


-- ## callback function on a NAT packet, comming from the network ##
onWarpPkt :: NATState -> Sink NATPkt -> Sink IPPkt -> NATPkt -> ADN ()

        -- # communication packet: IP traffic tunneling
onWarpPkt nat toWarp toTun (NATCom wid ipPkt) = do
    lM <- isLeased nat src      -- search for a lease register for the source IP
    case lM of
        Nothing -> nolease      -- no lease found
        Just wid' -> if wid == wid' then onWarp         -- the WID match the lease, send to TUN
                                  else leaseTaken wid'  -- the lease is register with a different WID
        where 
            onWarp = do
                refreshLease nat $ LeaseIP src
                toTun ipPkt
            src = iph_sourceIP $ ip_header ipPkt    -- the IP source of the encapsulated IP packet
            nolease = do  -- no Lease found: log warning, and reply error packet
                warning $ "received NAT com from a unknown user: " ++ show wid ++ " " ++ show src   
                toWarp $ NATErr wid LeaseUnknown 
            leaseTaken wid' = do    -- lease taken found: log warning, and reply error packet
                warning $ "received NAT com from user: " ++ show wid ++ " with a source IP leased to " ++ show wid'
                toWarp $ NATErr wid LeaseTaken

        -- # received a request for a lease from a client
onWarpPkt nat toWarp toTun (RequestLease wid) = do
    ipM <- hasLease nat wid     -- check if the client already has a lease
    case ipM of
        Just ip ->  toWarp $ OfferLease wid ip  -- if yes, send it back to the client
        Nothing -> do  
            ipM' <- allocateLease nat wid      -- if not, try to allocate a new IP 
            case ipM' of
                Nothing -> toWarp $ NATErr wid LeaseFailed      -- failed (all IP are taken): telling the client
                Just ip -> do debug $ "offering lease to user " ++ show wid ++ "  -> " ++ show ip
                              toWarp $ OfferLease wid ip        -- success: sending the client it's new IP!

        -- # a client freeing it's lease
onWarpPkt nat toWarp toTun (FreeLease wid) = do freeLease nat $ LeaseWID wid 
                                                debug $ "received FreeLease from user " ++ show wid

        -- # not supposed to happen: this is a server, receiving a lease offer!
onWarpPkt nat toWarp toTun (OfferLease wid ip) = warning $ "NAT router receveived a OfferLease packet from user: " ++ show wid ++ " -> " ++ show ip



-- # Routing packet from the TUN interface (from internet) to ADN
onTunPkt :: NATState -> Sink NATPkt -> Sink IPPkt -> IPPkt -> ADN ()
onTunPkt nat toWarp toTun ipPkt = do
        debug $ "receveived ip pkt for " ++ show dst
        lM <- isLeased nat dst  -- searck for a lease for the destination IP of the packet
        case lM of
            Nothing -> debug "no lease found"       -- no lease found
            Just wid -> toWarp $ NATCom wid ipPkt   -- tunnel the packet to the client having the lease
    where dst = iph_destIP $ ip_header ipPkt        


registerServerCLI :: NATState -> ADN ()
registerServerCLI state = do
        addOrderCLI_ "show" "display the current leases" showLeases
        addOrderCLI  "free" "free WID : free the specified lease" freeLeaseOrder
        addOrderCLI_ "freeAll" "free every IP lease" freeAllLeases
    where
        showLeases = concat . map showLease <$> getLeases state
        showLease (wid,ip) = "\t WID : " ++ show wid ++ "\t->\t" ++ show ip ++ "\n"
        freeLeaseOrder a = case readMaybe a of
                    Just wid -> freeLease state (LeaseWID $ WID wid) >> pure ("freeing lease for user " ++ show wid)
                    Nothing -> pure "unable to read argument... Syntax: free WID"
        freeAllLeases = do
            leases <- getLeases state
            res <- ("freeing all leases:\n" ++) <$> showLeases
            forM_ (snd <$> leases) $ freeLease state . LeaseIP
            return res
            

        
    





