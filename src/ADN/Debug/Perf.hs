{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ADN.Debug.Perf (
    Perf(..),
    PerfPkt
  
  ) where


import Prelude hiding (error)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Text.Read(readMaybe)
import Data.Time.Clock
import Data.Binary


import ADN.Core


-- | Perf is a very rudimentary performance testing unit: it can be used as a pipe extremity (PipeStartBuilder or PipeEnd), 
-- and implement 2 CLI commands start/stop allowing for bitrate measurement between 2 points (possibly in 2 different instances, ie between 2 computers).
-- WARNING: this module is very naively written and poorly tested. The results reported should be taken with a great caution!
data Perf = Perf String

-- | rudimentary bitrate measurement
instance PipeStartBuilder Perf PerfPkt where 
    pipeStart (Perf name) = block name $ pipeEndToStart $ startPerf

-- | rudimentary bitrate measurement
instance PipeEndBuilder Perf PerfPkt where
    pipeEnd (Perf name) c = block name $ startPerf c

-- | The type of packet exchanged by Perf modules.
data PerfPkt = PerfPkt {
    pktNum :: Int,
    pktPayload ::  Payload
}

-- ##### INTERNAL #####

data PerfState = PerfState {
    perfReceived :: ADNVar (Maybe [Int])
}

startPerf :: PipeEnd PerfPkt
startPerf c = do
    st <- PerfState <$> newADNVar Nothing
    registerPerfOrder st (send c)
    forkADN $ forever $ recv c >>= onPkt st 
        where 
            onPkt :: PerfState -> PerfPkt -> ADN ()
            onPkt st p = modifyADNVar_ v $ onPerfPkt p
                where v = perfReceived st

 
onPerfPkt :: PerfPkt -> Maybe [Int] -> ADN (Maybe [Int])
onPerfPkt p st = case st of Nothing -> logPkt []
                            Just l -> logPkt l
    where 
        logPkt :: [Int] -> ADN (Maybe [Int])
        logPkt l = do
             debug ("received PerfPkt num " ++ show i) 
             return (Just (i:l))
        i = pktNum p


registerPerfOrder (PerfState st) si = do
    addOrderCLI "start" "start sending packets to the destinary. Arguments are: number_of_packets packet_size (in bytes)" (startPerf si)
    addOrderCLI_ "stop" "stop the current performance test. No arguments" stopPerf
        where 
            startPerf si argsStr = maybe (pure "unable to parse arguments") launch testArgs
                where 
                    launch args = withADNVar st (startPerfTest si args)
                    args = words argsStr :: [String]
                    testArgs = if length args < 2 then Nothing else do
                        num <- readMaybe (args !! 0) 
                        size <- readMaybe (args !! 1)
                        Just (num,size)

            stopPerf = modifyADNVar st stopPerfTest


startPerfTest :: Sink PerfPkt -> (Int,Int) -> Maybe [Int] -> ADN String
startPerfTest _ _ (Just _) = pure "a test is already started"
startPerfTest si (num,size) Nothing = do
    let pay = genPayload (size - 1)
    info $ "Starting test for " ++ show num ++ " packets of size " ++ show (BS.length . BL.toStrict . encode $ PerfPkt 0 pay) ++ " bytes"
    t0 <- liftIO $ getCurrentTime
    forM_ [1..num] $ \i -> si (PerfPkt i pay)
    tF <- liftIO $ getCurrentTime
    let dt = diffUTCTime tF t0
        rate = (fromIntegral (num*size))/dt/1000000.0
    return $ "Total send "++ show (num*size) ++" in t = " ++ show dt ++ "\trate = " ++ show rate ++ "Mbyte/s"


stopPerfTest :: Maybe [Int] -> ADN (Maybe [Int], String)
stopPerfTest Nothing = pure (Nothing, "no test to stop")
stopPerfTest (Just l) = pure (Nothing, "stopping test. Received packet: " ++ show (length l) ++" / " ++ show (maximum l))


genPayload :: Int -> Payload
genPayload n = Payload $ BS.pack $ take n $ repeat 0
instance Binary PerfPkt where
    put (PerfPkt n p) = put (toEnum n :: Word8) >> put p
    get = PerfPkt <$> (fromEnum <$> (get :: Get Word8)) <*> get




