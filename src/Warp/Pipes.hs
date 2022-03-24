{-# LANGUAGE MultiParamTypeClasses #-}
module Warp.Pipes (
  Warp(..),
  makeWarp,

  WarpDispatcher,
  warpDispatcher,
  simpleWarp,
  ) where

import qualified Data.Map as M
import Control.Concurrent.Suspend
import Text.Read (readMaybe)
import qualified Data.Set as S

import ADN.Core
import ADN.Dispatcher

import Warp.Types
import Warp.Packet
import Warp.Routing
import Warp.Relay


type WarpDispatcher = Dispatcher WProtocol WarpMsg WarpMsg

simpleWarp :: Warp -> WProtocol -> Pipe WarpPkt WarpMsg
simpleWarp warp pID c = do
    wd <- warpDispatcher 
    ws <- makeWarp warp
    pure c >>= pipe ws >>= pipeEnd wd
    pipeStart $ Protocol wd pID


warpDispatcher :: ADN WarpDispatcher
warpDispatcher = simpleDispatcher popID putID
    where
        popID (WarpMsg uid p) = let (i,p') = readWord8 p  in (WProtocol i, WarpMsg uid p')
        putID (WProtocol i) (WarpMsg uid p') = let p = writeWord8 i p' in WarpMsg uid p



data Warp = Warp {
    myWarpID :: WID,        -- Warp identifier: should be unique among neighbours
    initialTTL :: TTL,
    edgeTimeout :: Int,         -- timeout delay for a graph edge.
    myRessources :: [RID]
}

instance PipeBuilder WarpState WarpPkt WarpMsg where
    pipe ws c = block "warp" $ makePipeState run onLowPacket onUpPacket c
        where run = addWarpCLI ws (send c) >> pure ws

-- ##### INTERNAL #####

onLowPacket :: WarpState -> Sink WarpPkt -> Sink WarpMsg -> WarpPkt -> ADN ()
onLowPacket ws toLow toUp pkt 
        | null $ _path pkt = warning "received WarpPacket with empty path"
        | otherwise = do
    insertPath ws (_identity ws : _path pkt)
    case _payload pkt of
        WarpSearch _ -> onSearchPkt ws toLow pkt
        WarpPayload _ -> relayPacket ws toLow onReceived pkt
        where onReceived p = case _payload p of
                                WarpPayload pl -> toUp $ WarpMsg sourceID pl
                                WarpSearch _ -> pure ()
                where sourceID = last $ _path p

onUpPacket :: WarpState -> Sink WarpPkt -> Sink WarpMsg -> WarpMsg -> ADN ()
onUpPacket ws toLow _ (WarpMsg dest pay) = do
    rM <- randomSearch ws (V dest)
    case rM of
        Nothing -> warning $ "unable to find road to destinary: " ++ show dest
        Just r -> do
            debug $ "Casting route: " ++ show r
            toLow $ WarpPkt [_identity ws] (Right [pathToRoute r]) $ WarpPayload pay


makeWarp :: Warp -> ADN WarpState
makeWarp (Warp me max_ttl eto myRessources) = do
        rs <- RessourceState <$> newADNVar (S.fromList myRessources) <*> newADNVar M.empty
        ws <- WarpState me max_ttl delay <$> newADNVar emptyGraph <*> pure rs
        return ws
    where
        delay = sDelay $ toEnum eto
        emptyGraph = G $ M.singleton (V me) M.empty

addWarpCLI :: WarpState -> Sink WarpPkt -> ADN ()
addWarpCLI ws toLower = do
    addOrderCLI "insertPath" "insert a path to the graph" addRoad
    addOrderCLI_ "showGraph"  "display the current graph"  showGraph
    addOrderCLI "search" "discover INT: try to discover a route to the specified ressource" discover
    addOrderCLI "addRessource" "addRessource INT: enable providing the specified ressource" addRID
    addOrderCLI_ "ressources" "shows the provided ressources" showRessourceState
    addOrderCLI_ "sources" "shows the available sources" showSources
        where
            showGraph :: ADN String
            showGraph = show <$> (readADNVar $ _graph ws)
                           
            addRoad :: String -> ADN String
            addRoad [] = pure "no path provided"
            addRoad s = case mapM readMaybe $ words s of
                Nothing -> pure "unable to read path"
                Just l -> do 
                    insertPath ws (map WID l) 
                    pure $ "path inserted: " ++ show l

            addRID s = case RID <$> readMaybe s of
                Nothing -> pure "unable to parse the ressource ID"
                Just rid -> addRessource ws rid >>  pure ("Ressource added : " ++ show rid)
            discover s = case RID <$> readMaybe s of
                Nothing -> pure "unable to parse the ressource ID"
                Just rid -> do toLower (discoverPkt ws rid) >> pure ("Research sent onto broadcast for ressource : " ++ show rid)

            showRessourceState = withADNVar (_myRessources $ _ressourcesState ws) $ \set -> pure $ show set
            showSources = withADNVar (_verticesRessources $ _ressourcesState ws) $ \vertices -> pure $ show vertices
