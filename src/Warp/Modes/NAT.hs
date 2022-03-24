module Warp.Modes.NAT (
    -- * NAT modes
    natClient,
    natServer,

    -- * Default structure
    DefaultClient(..), 
    DefaultServer(..), 
    )where

import Options.Applicative 
import System.Posix.Types hiding (WID)
import Control.Concurrent.Suspend

import Warp.Link
import Warp.Utils
import Warp.NAT
import Warp.Types
import Warp.Modes.Warp

import ADN.Core hiding (info)
import ADN.Modes


-- | setup a 'NATClient', routing internet traffic into ADN
--
-- the argument is the default configuration, if not override by the programs arguments.
natClient :: DefaultClient -> WarpMode
natClient c@(DefaultClient wp tun mtu to) = simpleWarpMode wp $ Mode "client" "route internet traffic into ADN" gen run
    where
        gen = root <$> parseNATClient c
        run = cliPipeEnd <$> parseNATClient c
        cliPipeEnd cli c = pure c >>= decoder >>= pipe cli >>= pipeEnd (natclIface cli)

-- | server" "setup a 'NATServer' to share a internet connexion over ADN
--
-- the argument is the default configuration, if not override by the programs arguments.
natServer :: DefaultServer -> WarpMode
natServer s@(DefaultServer wp tun mtu ip start stop to) = simpleWarpMode wp $ Mode "server" "share an internet access over ADN" gen run
    where
        gen = root <$> parseNATServer s
        run = srvPipeEnd <$> parseNATServer s
        srvPipeEnd srv c = pure c >>= decoder >>= pipe srv >>= pipeEnd (natsIface srv)


-- ##### PARSING #####

parseNATClient :: DefaultClient -> Parser NATClient
parseNATClient (DefaultClient _ tun mtu to) = NATClient <$> parseScriptName <*> parseTun tun mtu <*> parseTimeout
    where 
        parseScriptName = strOption (long "client-script" <> metavar "F" <> value "configure_client" <> showDefault <> help "name of the runtime script used by the NAT client")
        parseTimeout = sDelay . read <$> strOption (long "nat-timeout" <> metavar "TIMEOUT" <> value (show $ to) <> showDefault <> help "time after which a server is deleted, if it hasn't been refreshed")

parseNATServer :: DefaultServer -> Parser NATServer
parseNATServer (DefaultServer wp tun mtu ip start stop to) = 
        NATServer <$> parseTun tun mtu <*> parseGateway <*> parseIP <*> parseRange <*> parseTimeout
    where
        parseGateway = strOption (long "gateway" <> metavar "INTERFACE"  <> help "enables the NAT server and routes paquets into INTERFACE")
        parseIP = readAddr <$> strOption (long "server-ip" <> metavar "IP"  <> value ip <> showDefault <> help "IP address of the NAT server, in the virtual LAN")
        parseRange = (,) <$> (readAddr <$> strOption (long "nat-start" <> metavar "IP"  <> value start <> showDefault <> help "first address leased by the NAT server"))
                         <*> (readAddr <$> strOption (long "nat-stop"  <> metavar "IP"  <> value stop  <> showDefault <> help "last address leased by the NAT server" ))
        parseTimeout = sDelay . read <$> strOption (long "nat-timeout" <> metavar "TIMEOUT" <> value (show $ to) <> showDefault <> help "time after which a lease is deleted, if it hasn't been refreshed")


-- | Parse th interface name and the MTU for an 'ADNTun' virtual interface.
parseTun :: String -> Int -> Parser ADNTun
parseTun iface mtu = ADNTun 
        <$> strOption (long "tun-iface" <> metavar "INTERFACE" <> value iface <> showDefault <> help "adn virtual interface")
        <*> (read <$> strOption (long "tun-mtu" <> metavar "MTU" <> value (show mtu) <> showDefault <> help "maximum size of packets"))


-- | Relevant default parameters for 'warpClient' mode
data DefaultClient = DefaultClient {
    cliWarpProtocolID :: WProtocol,   -- ^ Warp-protocolID used by the NAT
    cliTunName :: String,             -- ^ name of the virtual interface created
    cliTunMTU :: Int,                 -- ^ maximum size of packet read from the virtual interface.
    cliNATTimeout :: Int              -- ^ lease timeout 
}

-- | Relevant default parameters for 'warpServer' mode
data DefaultServer = DefaultServer {
    srvWarpProtocolID :: WProtocol,   -- ^ Warp-protocolID used by the NAT
    srvTunName :: String,             -- ^ name of the virtual interface created
    srvTunMTU :: Int,                 -- ^ maximum size of packet read from the virtual interface.
    srvNATIP :: String,               -- ^ IP address of the Server in the NAT
    srvNATStart :: String,            -- ^ first IP leased by the server
    srvNATStop :: String,             -- ^ last IP leased by the server
    srvNATTimeout :: Int              -- ^ lease timeout 
} 



