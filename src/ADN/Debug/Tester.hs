-- | This module allows the creating of simulated networks of ADN clients.
-- for now, only static networks is implemented: the matrix of connexion is specified at the beginning of the test.
module ADN.Debug.Tester (
  ClientID, TestClient(..),
  runTest, fullGraphTest, chainTest,
  ) where

import Prelude hiding(error)
import Control.Monad
import qualified Data.Map as M

import ADN.Core


type ClientID = Int

-- | TestClient represent a single ADN client: it consist of a pipeline used to process packets from other clients,
-- and the list of clients in range.
data TestClient = TestClient {
    tcPipeline :: PipeEnd Payload,
    tcNeighs :: [ClientID]
}


-- | this function start every clients in the list, and dispatch the packets emmitted by a client to its neighbours. 
runTest :: [TestClient] -> ADN ()
runTest clients = makeTest >>= runT
    where
        makeTest = M.fromList <$> sequence clientList
        clientList = zipWith makeClient [1..] clients 
        makeClient :: ClientID -> TestClient -> ADN (ClientID,Client)
        makeClient i (TestClient p neighs) = block (show i) $ do
            (c1,c2) <- connectedChannels
            p c1
            return (i, Client (show i) c2 neighs)
            

        runT :: Test -> ADN ()
        runT t = mapM_ (runClient t) t

        runClient :: Test -> Client ->  ADN ()
        runClient t (Client name chan neighs) = block name runC
            where 
                runC = linkSinkToSource sendToNeighs (recv chan) :: ADN ()
                sendToNeighs pkt = forM_ neighs $ sendToNeigh pkt
                sendToNeigh pkt i = case M.lookup i t of
                    Nothing -> error $ "attempted to send to unknown testClient: " ++ show i
                    Just c -> send (clientChan c) $ pkt
            
-- | simple wrapper around runTest: every client is connected to every other 
fullGraphTest :: Int -> [PipeEnd Payload] -> ADN ()
fullGraphTest n ps = runTest $ zipWith TestClient ps $ map neighs [1..n]
    where neighs i = [1..(i-1)]++[(i+1)..n]

-- | simple wrapper around runTest: clients are connected along a chain: 1 <-> 2 <-> ... <-> n-1 <-> n
chainTest :: Int -> [PipeEnd Payload] -> ADN ()
chainTest n ps = runTest $ zipWith TestClient ps $ map neighs [1..n]
    where 
        neighs i | i == 1 = [2]
                 | i == n =  [n-1]
                 | otherwise =  [i-1, i+1] 


-- ##### INTERNAL #####

data Client = Client {
    clientName :: String,
    clientChan :: Channel Payload,
    clientNeigh :: [ClientID]
}

type Test = M.Map ClientID Client






