{-# LANGUAGE DeriveGeneric #-}
module Data.IP.Header(
  -- * High-level IP Headers
  IPVersion(..), IPProtocol(..), IPFlags(..), IPHeader(..),
  -- * Low-level (raw) IP headers, for sending
  IPHeader_Raw, header2Raw, raw2Header ) where

import Data.Word
import Data.Binary
import Data.Binary.Get (getWord16be)
import Data.Binary.Put (putWord16be)
import GHC.Generics
import Data.Bits

import Data.IP.Addr

newtype Word16BE = Word16BE { unWord16BE :: Word16 }

instance Binary Word16BE where
  get = Word16BE <$> getWord16be
  put = putWord16be . unWord16BE

data IPHeader_Raw = IPHeader_Raw
  {
    iph_raw_version_helen        :: !Word8,
    iph_raw_serviceType          :: !Word8,
    iph_raw_totalLength          :: !Word16BE,
    iph_raw_identification       :: !Word16BE,
    iph_raw_flags_fragmentOffset :: !Word16BE,
    iph_raw_ttl                  :: !Word8,
    iph_raw_protocol             :: !Word8,
    iph_raw_checksum             :: !Word16BE,
    iph_raw_sourceIP             :: !IP,
    iph_raw_destIP               :: !IP
  }
  deriving Generic

instance Binary IPHeader_Raw

data IPVersion = IPv4 | IPv6 | OtherVersion !Word8
  deriving Show
data IPFlags = LastFragment | SomeFragment | Unfragmented
  deriving Show
data IPProtocol = TCP | UDP | ICMP | OtherProto !Word8
  deriving Show

data IPHeader = IPHeader
  {
    iph_version              :: !IPVersion,
    iph_totalLength          :: !Word16,
    iph_identification       :: !Word16,
    iph_flags                :: !IPFlags,
    iph_fragmentOffset       :: !Word16,
    iph_ttl                  :: !Word8,
    iph_protocol             :: !IPProtocol,
    iph_sourceIP             :: !IP,
    iph_destIP               :: !IP
  }
  deriving Show

instance Binary IPHeader where
    put p = put $ header2Raw p
    get = raw2Header <$> get

header2Raw (IPHeader v l id f off ttl proto src dst) =
  IPHeader_Raw helen 0 (Word16BE l) (Word16BE id) (Word16BE flagoff) ttl rawproto (Word16BE chk) src dst
  where helen = (vers `shiftL` 4) .|. 5
          where vers = case v of
                  IPv4 -> 4
                  IPv6 -> 6
                  OtherVersion x -> x
        flagoff = off .|. (fl `shiftL` 13)
          where fl = case f of
                  LastFragment -> 4
                  SomeFragment -> 6
                  Unfragmented -> 0
        rawproto = case proto of
          ICMP -> 1
          TCP -> 6
          UDP -> 17
          OtherProto p -> p
        chk = complement $ fromIntegral $ (s .&. 0xFFFF) + (s `shiftR` 16)
          where (srcA,srcB,srcC,srcD) = ipToTuple src
                (dstA,dstB,dstC,dstD) = ipToTuple dst
                s :: Word32
                s = sum [ fromIntegral helen `shiftL` 8,
                          fromIntegral l,
                          fromIntegral flagoff,
                          fromIntegral id,
                          fromIntegral ttl `shiftL` 8,
                          fromIntegral rawproto,
                          fromIntegral srcA `shiftL` 8,
                          fromIntegral srcB,
                          fromIntegral srcC `shiftL` 8,
                          fromIntegral srcD,
                          fromIntegral dstA `shiftL` 8,
                          fromIntegral dstB,
                          fromIntegral dstC `shiftL` 8,
                          fromIntegral dstD
                        ]

raw2Header (IPHeader_Raw helen _ (Word16BE l) (Word16BE id) (Word16BE flagoff) ttl rawproto _ src dst) =
  IPHeader v l id f off ttl proto src dst
  where v = case helen `shiftR` 4 of
          4 -> IPv4
          6 -> IPv6
          x -> OtherVersion x
        f = case flagoff `shiftR` 13 of
          4 -> LastFragment
          6 -> SomeFragment
          _ -> Unfragmented
        off = flagoff .&. 0x1FFF
        proto = case rawproto of
          1 -> ICMP
          6 -> TCP
          17 -> UDP
          x -> OtherProto x
          
  
