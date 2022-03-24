{-# LANGUAGE DeriveGeneric #-}
module Warp.NAT.Packet (
    NATPkt(..),
    NATError(..)
) where


import Prelude hiding(error)
import GHC.Generics
import qualified Data.Map as M
import Network.Socket
import Control.Lens
import Control.Monad.Trans
import Data.Word
import Data.List
import Data.Binary
import qualified Data.ByteString as BS
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import System.Process (callProcess)

import Warp.Types hiding (_timer)
import ADN.Core
import ADN.Debug
import Data.IP.Packet
import Data.IP



-- | Type of packet exchange with the lower level (Warp, probably)
data NATPkt = NATCom WID IPPkt      -- ^ communication packet: encapsulated IP packets
            | RequestLease WID      -- ^ request an IP from a server
            | FreeLease WID         -- ^ free an IP from a server
            | OfferLease WID IP     -- ^ offer an IP to a client
            | NATErr WID NATError   -- ^ something went wrong

-- | Error code over the NAT
data NATError = LeaseUnknown        -- ^ tried to send NATCom to a server, but no lease registered there
              | LeaseFailed         -- ^ the server was unable to provide an IP (they are all taken!)
              | LeaseTaken          -- ^ the server has leased this IP, but to a different WID


instance Binary NATPkt where
    put (NATCom uid pkt)    = put uid >> put8 0 >> put pkt
    put (NATErr uid e)      = put uid >> put8 1 >> put e
    put (RequestLease uid)  = put uid >> put8 2 
    put (FreeLease uid)     = put uid >> put8 3 
    put (OfferLease uid ip) = put uid >> put8 4 >> put ip
    
    get = do
        uid <- get
        n <- get :: Get Word8
        case n of
            0 -> NATCom uid <$> get
            1 -> NATErr uid <$> get
            2 -> return $ RequestLease uid
            3 -> return $ FreeLease uid
            4 -> OfferLease uid <$> get

instance Binary NATError where
    put LeaseUnknown    = put8 0
    put LeaseFailed     = put8 1
    put LeaseTaken      = put8 2
    get = do
        n <- get :: Get Word8
        case n of 
            0 -> pure LeaseUnknown
            1 -> pure LeaseFailed 
            2 -> pure LeaseTaken

-- | NO READ IMPLEMENTED
instance DebugPacket NATPkt where
    dbgRead str = Nothing 
    dbgShow _ = "NAT Packet"


put8 a = put (a :: Word8)


