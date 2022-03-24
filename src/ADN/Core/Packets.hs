module ADN.Core.Packets (
  -- * Payload
  Payload(..),readWord8,writeWord8,
  packetLength,

  -- * decoder
  decoder,
  ) where
import Prelude hiding (error)
import Data.Binary
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import ADN.Core.Block 
import ADN.Core.Pipe

-- | A 'Payload' is a simple wrapper around strict 'Data.ByteString.ByteString'
--
-- The Binary instance does not encapsulate the content : 
-- this type can only be used as the last field of a data structure, 
-- as it will consume every remaining bytes when decoding.
data Payload = Payload {getPayload :: !BS.ByteString}
    deriving (Eq, Ord)

instance Binary Payload where
    put (Payload bs) = P.putLazyByteString $ BL.fromStrict bs
    get = Payload . BL.toStrict <$> G.getRemainingLazyByteString
    

-- | A simple pipe, allowing to convert any kind of packet to any other, as long as they are instances of Binary.
-- The user is responsible for using it on the correct kind of packet!
--
-- Should work fine for decoding headers from payload, but it's probably not suited to cast complex data-types from
-- one another, as it uses " decode . encode " calls.
decoder :: (Binary p, Binary p') => Pipe p p'
decoder = pure . decodeChannel

-- | Size of a 'Payload' Packet, in bytes
packetLength :: Payload -> Int
packetLength = BS.length . getPayload

-- | Read a byte (Word8) from a 'Payload', returning it as an Int value, and the remaining ByteString.
readWord8 :: Payload -> (Int,Payload)
readWord8 (Payload p) = (fromEnum $ BS.head p, Payload $ BS.tail p)

-- | Append a byte (Word8) to the beginning of a 'Payload'.
writeWord8 :: Int -> Payload -> Payload
writeWord8 i (Payload p) = Payload $ BS.append (BS.singleton $ toEnum i) p



-- ##### INTERNAL #####

decodeChannel :: (Binary p, Binary p') =>  Channel p -> Channel p'
decodeChannel (Channel si so) = Channel (decodeSink si) (decodeSource so)

decodeSink :: (Binary p, Binary p') =>  Sink p -> Sink p'
decodeSink f p = decodePacket p >>= maybe (pure ()) f

decodeSource :: (Binary p, Binary p') => Source p -> Source p'
decodeSource so = so >>= decodePacket >>= retry
    where retry Nothing = decodeSource so
          retry (Just a) = pure a

decodePacket :: (Binary p, Binary p') => p -> ADN (Maybe p')
decodePacket p = do
    case decodeOrFail $ encode p of
        Left err -> do error $ "unable to decode packet:" ++ show err
                       pure Nothing
        Right (remain, numRead,a) -> do
                        pure $ Just a 



