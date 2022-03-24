module Conf where

import ADN.Core
import Warp
import Modes


-- ##### DEFAULT CONFIGURATION #####


    -- ## Wlan IBSS configuration
    
wlan_freq    = 2437  :: Int       -- frequency (in MHz)
wlan_ESSID   = "ADN"             -- ESSID of the network

    -- ## UD P/IP configuration 

link_IP      = "10.10.0.1"       -- IP adress on the link layer
link_brd     = "10.10.0.255"     -- broadcast adress 
link_port    = 5000  :: Int       -- UDP port used for the link
link_mtu     = 1500  :: Int       -- Maximum size of packets exchanged with the link interface

    -- ## Wa rp configuration

warp_LPID    = 66 :: Int          -- protocol ID used by warp on the link layer
warp_timeout = 300   :: Int       -- timeout of a edge in the graph (in s) 
warp_ttl     = 10 :: Int          -- maximum number of hop done by search packets

    -- ## ge neral configuration

adn_iface    = "adn0"            -- name of the virtual interface created to route packet from/to ADN
adn_MTU      = 1400  :: Int       -- MTU of the virtual interface: 
nat_WPID   =  WProtocol 255      -- Warp-port number used for the NAT tunneling

    -- ## Se rver configuration

nat_IP       = "10.10.254.1"     -- IP adress of the Server (gateway) in the virtual LAN
nat_start    = "10.10.254.2"     -- range of IP adresses offered by the Server to Clients.
nat_stop     = "10.10.254.254"
nat_timeout  = 300  :: Int       -- timeout (in s) of a IP lease


defaultLink = DefaultLink wlan_freq wlan_ESSID link_IP link_brd link_port link_mtu                  :: DefaultLink
defaultWarp = DefaultWarp warp_LPID Nothing warp_ttl warp_timeout                                   :: DefaultWarp
defaultClient = DefaultClient nat_WPID adn_iface adn_MTU nat_timeout                                :: DefaultClient
defaultServer = DefaultServer nat_WPID adn_iface adn_MTU nat_IP nat_start nat_stop nat_timeout      :: DefaultServer 
defaultTester = DefaultTester 4 60 4 $ WProtocol 0                                                  :: DefaultTester 

