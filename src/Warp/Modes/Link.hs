module Warp.Modes.Link (
    -- * link dispatcher
    LinkMode,
    runLinkModes,

    -- * Parsing arguments
    parseLink, 

    -- * Default structure
    DefaultLink(..),
    )where

import Options.Applicative 

import ADN.Core hiding (info)
import ADN.Modes
import ADN.Dispatcher

import Warp.Link
import Warp.Utils

-- | Represent a protocol registered on the link 'Dispatcher'
type LinkMode = Mode (DspOutput Int Payload)

-- | Generate a 'ADNMode' from a default 'LinkUDP' configuration, and a list of 'LinkMode'
--
-- each 'LinkMode' correspond to a possible protocol on link: the user choses at runtime which are used. 
runLinkModes :: DefaultLink -> [LinkMode] -> ADNMode
runLinkModes l lmds = runLink <$> manyModes lmds mode
    where 
        mode = Mode "link-UDP" "connect to a physical interface with a UDP socket" genLink (parseLink l)
        runLink :: (LinkUDP , [DspOutput Int Payload])-> ADN ()
        runLink (link, outputs) = pipeStart link >>= runDispatcher byteDispatcher outputs
        genLink = root <$> parseLink l

-- | Parse a 'LinkUDP' configuration from the options.
--
-- the argument is the default configuration, if not override by the programs arguments.
parseLink :: DefaultLink -> Parser LinkUDP
parseLink (DefaultLink freq essid ip brd port mtu) = LinkUDP <$> parsePhyConf <*> parseIPConf 
    where 
        parseIPConf = IPConf
            <$> (readAddr <$> strOption (long "phy-ip" <> metavar "ADDR" <> value ip <> showDefault <> help "The physical UDP address to bind WARP on"))
            <*> (readAddr <$> strOption (long "phy-brd" <> metavar "ADDR" <> value brd <> showDefault <> help "The broadcast address from which UDP WARP Packet will be sent"))
            <*> (read <$> strOption (long "phy-port" <> metavar "PORT_NUMBER" <> value (show port) <> showDefault <> help "The port number from which UDP WARP packet will be sent"))
            <*> (read <$> strOption (long "phy-mtu" <> metavar "MTU" <> value (show mtu) <> showDefault <> help "Maximum size of the packet on the WARP network."))
        parsePhyConf = parseEth <|> parseIBSS 
        parseEth = Eth <$> strOption (long "eth-iface" <> metavar "INTERFACE" <> help "ethernet physical interface to bind WARP on")
        parseIBSS  = IBSS 
            <$> strOption (long "wlan-iface" <> metavar "INTERFACE" <> help "Wireless ad-hoc interface")
            <*> (read <$>  strOption (long "wlan-freq" <> metavar "FREQUENCE" <> value (show freq) <> showDefault <> help "the frequence of the ad-hoc network"))
            <*> strOption (long "wlan-ssid" <> metavar "SSID" <> value essid <> showDefault <> help "the ssid of the ad-hoc network")

                           


-- | Relevant default parameters for a 'LinkUDP' configuration.
data DefaultLink = DefaultLink {
    dlFreq :: Int,                    -- ^ Wifi frequency, used for IBSS configuration ony
    dlESSID :: String,                -- ^ Wifi ESSID, used for IBSS configuration only
    dlIP :: String,                   -- ^ my IP adress 
    dlBrd :: String,                  -- ^ the network broadcast
    dlPort :: Int,                    -- ^ UDP port used
    dlMTU :: Int                      -- ^ maximum size of the datagrams
} 


