{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ADN.Debug.Debugger (
  -- * Debugger pipes
  Debugger,dbg,
  
  -- * Utilities
  DebugPacket(..)
  ) where

import Prelude hiding (error)
import Data.List(words)
import qualified Data.ByteString.UTF8 as BSU
import Text.Read

import ADN.Core


-- | A Debugger can be used as a pipeline extremity (PipeStartBuilder or PipeEnd), and allows to send directy packet into the pipeline with the CLI.
-- A Debug log is sent every time a packet is received by the Debugger. You can provide the Reading and Showing function yourself, or use the 
-- type class DebugPacket, and simply call the function dbg.
data Debugger p = Debugger {
    dbgName :: String,
    dbgReading :: String -> Maybe (String, p),
    dbgShowing :: p -> Maybe String
}

-- | send and receive message through CLI
instance PipeStartBuilder (Debugger p) p where
    pipeStart conf = dbgBlock conf $ pipeEndToStart (startDebugger conf)

-- | send and receive message through CLI
instance PipeEndBuilder (Debugger p) p where
    pipeEnd conf c = dbgBlock conf $ startDebugger conf c 
                

-- | create a Debugger with the provided name, using the methods of DebugPacket.
dbg :: DebugPacket a => String -> Debugger a
dbg name = Debugger name enc dec
    where
        enc s =  f <$> dbgRead s
        f d = (dbgShow d, d)
        dec p = Just $ dbgShow p

-- | Class of the packets recognized by the debugger: 
-- - dbgRead is the parsing function, used to forge packet from the CLI arguments.
-- - dbgShow is a simple show function, useful for having custom displays inside the debugger.
class DebugPacket a where 
    dbgRead :: String -> Maybe a
    dbgShow :: a -> String

-- | read and write a Payload as a utf8 string, using 'BSU.fromString' and 'BSU.toString'
instance DebugPacket Payload where 
    dbgRead = Just . Payload . BSU.fromString
    dbgShow = BSU.toString . getPayload

-- | identity
instance DebugPacket String where 
    dbgRead = Just
    dbgShow s = s


-- ##### INTERNAL #####

dbgBlock (Debugger n _ _) a = if null n then a else block n a

startDebugger :: Debugger p -> Channel p -> ADN ()
startDebugger conf c = do
    debug $ "starting debugger: " ++ dbgName conf
    linkSinkToSource logRecv $ recv c
    addOrderCLI  "send" "sends an encoded String in the pipeline" $ sendMsg
        where
            logRecv pkt = case dbgShowing conf pkt of
                Nothing -> error $ "Unable to decode received paquet"
                Just msg -> info $ "Packet received:\t" ++ msg ++ "\n"

            sendMsg :: String -> ADN String
            sendMsg msg = case dbgReading conf msg of
                Nothing -> pure $  "Unable to encode message: " ++ msg
                Just (ret,pkt) -> send c pkt >> pure ("Packet sent:\t" ++ ret)



