module Conf where

import Warp
import Modes


-- ##### DEFAULT CONFIGURATION #####


    -- ## Wlan IBSS configuration
    
wlan_freq :: Int
wlan_freq    = 2437              -- frequency (in MHz)
wlan_ESSID :: String
wlan_ESSID   = "ADN"             -- ESSID of the network

    -- ## UD P/IP configuration 

link_IP :: String
link_IP      = "10.10.0.1"       -- IP adress on the link layer
link_brd :: String
link_brd     = "10.10.0.255"     -- broadcast adress 
link_port :: Int
link_port    = 5000              -- UDP port used for the link
link_mtu :: Int
link_mtu     = 1500              -- Maximum size of packets exchanged with the link interface

    -- ## Wa rp configuration

warp_LPID :: Int
warp_LPID    = 66                -- protocol ID used by warp on the link layer
warp_timeout :: Int
warp_timeout = 300               -- timeout of a edge in the graph (in s) 
warp_ttl :: Int
warp_ttl     = 10                -- maximum number of hop done by search packets

    -- ## ge neral configuration

adn_iface :: String 
adn_iface    = "adn0"            -- name of the virtual interface created to route packet from/to ADN
adn_MTU :: Int
adn_MTU      = 1400              -- MTU of the virtual interface: 
nat_WPID :: WProtocol 
nat_WPID   =  WProtocol 255      -- Warp-port number used for the NAT tunneling

    -- ## Se rver configuration

nat_IP :: String 
nat_IP       = "10.10.254.1"     -- IP adress of the Server (gateway) in the virtual LAN
nat_start :: String 
nat_start    = "10.10.254.2"     -- range of IP adresses offered by the Server to Clients.
nat_stop :: String 
nat_stop     = "10.10.254.254"
nat_timeout :: Int
nat_timeout  = 300               -- timeout (in s) of a IP lease


defaultLink :: DefaultLink
defaultLink = DefaultLink wlan_freq wlan_ESSID link_IP link_brd link_port link_mtu

defaultWarp :: DefaultWarp
defaultWarp = DefaultWarp warp_LPID Nothing warp_ttl warp_timeout

defaultClient :: DefaultClient
defaultClient = DefaultClient nat_WPID adn_iface adn_MTU nat_timeout

defaultServer :: DefaultServer
defaultServer = DefaultServer nat_WPID adn_iface adn_MTU nat_IP nat_start nat_stop nat_timeout

defaultTester :: DefaultTester
defaultTester = DefaultTester 4 60 4 $ WProtocol 0

