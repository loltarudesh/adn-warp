module Data.IP.Packet (
  -- * Pools of IP addresses
  IPPkt (..), parseIP, parseHeader,parsePkt
  ) where


import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Lazy  (toStrict)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.IP
import Data.IP.Header
import ADN.Core
import ADN.Debug

-- | newtype to avoid confusion with ADN Payload Packets...
-- a IPPkt is a simple ByteString, but incoded as a standard IP packet (not a ADN packet)
data IPPkt = IPPkt {  ip_header :: IPHeader ,  ip_payload :: Payload }
instance Binary IPPkt where
    put (IPPkt h p) = put h >> put p
    get = IPPkt <$> get <*> get

-- | syntax = <src> <dst>
-- | example: 192.168.2.1
instance DebugPacket IPPkt where 
    dbgRead str = case parse parsePkt "" str of
                        Left err -> Nothing
                        Right v -> Just v 
    dbgShow (IPPkt hdr pl) = show hdr ++ " :" ++ dbgShow pl


parseIP :: Parser IP
parseIP = do
        a <- num
        char '.'
        b <- num
        char '.'
        c <- num
        char '.'
        d <- num
        pure $ tupleToIP (read a,read b,read c,read d)
    where num = many1 digit
parseHeader = do
        src <- parseIP
        spaces
        dst <- parseIP
        spaces
        pure $ IPHeader IPv4 0 0 LastFragment 0 0 UDP src dst
parsePkt = do
    hdr<- parseHeader 
    pl <- many (alphaNum <|> space)
    eof
    pure $ IPPkt hdr (Payload $ toStrict $ encode $ pl)


