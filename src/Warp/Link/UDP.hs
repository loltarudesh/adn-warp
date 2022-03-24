{-# LANGUAGE MultiParamTypeClasses #-}
-- | Links to the physical layer, through an UDP socket. 
--
-- supported modes: ethernet | Wifi-IBSS | user-configured
module Warp.Link.UDP (
  LinkUDP(..), PhyConf(..), IPConf(..),

  ) where

import Control.Monad.IO.Class
import Network.Socket 
import Data.ByteString as BS
import Network.Socket.ByteString as S

import Data.IP

import ADN.Core
import Warp.Utils

{-| IP configuration of the interface -}
data IPConf = IPConf {me :: IP,   -- ^ the ip address of the adapter
                      broadcast :: IP, -- ^ the broadcast address
                      port :: PortNumber, -- ^ the port dedicated to the warp network
                      mtu :: Int  -- ^ the maximum size of a packet
}
    deriving Show

-- | Physical configuration of the interface.
data PhyConf = 
    -- | use this if the physical link is already up and configured, and you don't wan't ADN to change the setup
    AleadyConfigured 
    -- | setup a simple direct link over a ethernet cable
    | Eth { 
        _ethIface :: String         -- ^ name of the ethernet interface      
}   -- | setup a Wifi adapter in IBSS (ad-hoc) mode
    | IBSS {
        _wlanIface :: String,       -- ^ name of the wireless Wifi interface
        _wlanFreq :: Int,           -- ^ Frequency of the IBSS network
        _wlanSSID :: String         -- ^ ESSID of the IBSS network
}

-- | receive the content of the UDP datagrams, if the source IP is different than ours.
-- outgoing 'Payload' are sent to the broadcast. 
--
-- WARNING: the upper layer is in charge
-- of the fragmentation: the 'Payload', encapsulated in the UDP/IP header, should be 
-- smaller than the set MTU.
data LinkUDP = LinkUDP PhyConf -- ^ physical configuration
                       IPConf  -- ^ IP configuration for the UDP socket


-- | a link to the physical layer, through an UDP socket. 
instance PipeStartBuilder LinkUDP Payload where
    pipeStart (LinkUDP _ ipconf) = do
        sock <- liftIO $ mkSock ipconf
        registerCloseAfter $ do
            debug "closing sock"
            liftIO $ close $ getSock sock
        (c1,c2) <- connectedChannels
        linkChannels c1 $ makeChannel sock
        return c2


-- | Configure the selected interface in the correct mode, and set the IP, broadcast, and MTU
instance RootAccess LinkUDP where 
    root (LinkUDP (IBSS wl freq ssid) ipconf) = [set, close]
        where 
            set = RootBefore "Configuring Wifi link in IBSS mod" 
               ["ip link set " ++ wl ++ " down",
                "ip link set " ++ wl ++ " mtu " ++ show (mtu ipconf),
                "iw " ++ wl ++ " set type ibss",
                "ip link set " ++ wl ++ " up",
                "ip addr add " ++ show (me ipconf) ++ "/24 brd " ++ show (broadcast ipconf) ++ " dev " ++ wl,
                "iw dev " ++ wl ++ " ibss join " ++ ssid ++ " " ++ show freq]

            close = RootAfter "Closing wifi interface" 
               ["ip link set " ++ wl ++ " down",
                "ip addr flush " ++ wl]


    root (LinkUDP (Eth eth) ipconf) = [set, close]
        where 
            set = RootBefore "Configuring ethernet interface"
               ["ip link set " ++ eth ++ " up",
                "ip link set " ++ eth ++ " mtu " ++ show (mtu ipconf),
                "ip addr add " ++ show (me ipconf) ++ "/24 brd " ++ show (broadcast ipconf) ++ " dev " ++ eth]
            close = RootAfter "Closing ethernet interface"
               ["ip link set " ++ eth ++ " down",
                "ip addr flush " ++ eth]
    

-- ##### INTERNAL #####

makeChannel sock = Channel sender receiver
    where sender pkt = do
                bytes <- liftIO $ sendPacket sock pkt
                debug $ show bytes ++ " have been sent"
          receiver = do
                p <- liftIO $ recvPacket sock
                let bytes = BS.length $ getPayload p
                debug $ show bytes ++ " have been received"
                return p
            

{-| Descriptor to the lower network |-}
data ADNSocket = ADNSocket { getSock :: Socket,
                             getLocalIp :: SockAddr,
                             getBroadcast :: SockAddr,
                             getMTU :: Int }

mkSock :: IPConf -> IO ADNSocket
mkSock c@(IPConf me brd warpport mtu) = do 
        s <- socket AF_INET Datagram 17
        bind s mybroadcast
        setSocketOption s Broadcast 1
        pure $ ADNSocket s myip mybroadcast mtu
    where myip = SockAddrInet warpport $ tupleToHostAddress $ extractAddr (show me)
          mybroadcast = SockAddrInet warpport $ tupleToHostAddress $ extractAddr (show brd)

{-  -- TODO: this would be much better, but does not works!! endianess, probably... fucking endians!
    where myip = SockAddrInet warpport $ unIP me 
          mybroadcast = SockAddrInet warpport $ unIP brd
-}



sendPacket :: ADNSocket -> Payload -> IO Int
sendPacket (ADNSocket sock _ broadcast mtu ) (Payload payload) = S.sendTo sock payload broadcast

recvPacket :: ADNSocket -> IO Payload
recvPacket adn@(ADNSocket sock localip broadcast mtu) = do
            (pkt, addr) <- liftIO $ S.recvFrom sock mtu
            if addr == localip then recvPacket adn else pure (Payload pkt)


