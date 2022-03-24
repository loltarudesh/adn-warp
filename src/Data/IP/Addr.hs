module Data.IP.Addr (
    IP(..), ipToTuple, tupleToIP
    ) where



import Data.Word
import Data.Bits
import Data.List
import Data.Binary.Get (getWord32be)
import Data.Binary.Put (putWord32be)
import Data.Binary


newtype IP = IP { unIP :: Word32 }
  deriving (Eq,Ord,Read)
instance Binary IP where
  get = IP <$> getWord32be
  put = putWord32be . unIP

instance Show IP where show ip = intercalate "." $ show <$> [a,b,c,d]
                         where (a,b,c,d) = ipToTuple ip

ipToTuple :: IP -> (Word8,Word8,Word8,Word8)
ipToTuple (IP ip) = (fromIntegral (ip`shiftR`24),
                     fromIntegral (ip`shiftR`16),
                     fromIntegral (ip`shiftR`8),
                     fromIntegral ip)

tupleToIP :: (Word8,Word8,Word8,Word8) -> IP
tupleToIP (a,b,c,d) = IP $ sum [fromIntegral a `shiftL` 24, fromIntegral b `shiftL` 16, fromIntegral c `shiftL` 8, fromIntegral d]


