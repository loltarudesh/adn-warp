{-# LANGUAGE DeriveGeneric #-}
module Warp.Routing.Search ( 
    onSearchPkt,
    addRessource,
    discoverPkt,

) where 

import Prelude hiding (error)
import GHC.Generics
import Data.Binary
import Warp.Types
import ADN.Core
import Control.Lens
import Data.List.Lens
import qualified Data.Map as M
import Data.Maybe
import Data.Tree
import qualified Data.Set as S 
import Warp.Routing.Graph
import Warp.Routing.Route
import qualified Data.ByteString.Lazy as BL
import Control.Monad


addRessource :: WarpState -> RID -> ADN ()
addRessource ws rid = modifyADNVar_ (_myRessources $ _ressourcesState ws) $ \set -> pure $ S.insert rid set

discoverPkt :: WarpState -> RID -> WarpPkt
discoverPkt ws rid = WarpPkt [_identity ws] (Left initial_ttl) (WarpSearch (Ask $ Res rid))
    where initial_ttl = _initialTTL ws                 

onSearchPkt :: WarpState -> Sink WarpPkt ->  WarpPkt -> ADN ()
onSearchPkt ws toLower pkt = case pkt of
        WarpPkt  _ _ (WarpSearch (Ask _))  -> onAsk ws pkt >>= sendMaybe
        WarpPkt  _ _ (WarpSearch (Tell _)) -> onTell ws pkt >>= sendMaybe
    where sendMaybe Nothing = pure ()
          sendMaybe (Just pkt) = toLower pkt



onTell :: WarpState -> WarpPkt -> ADN (Maybe WarpPkt)
onTell _ (WarpPkt [] _ _)  = error "onTell: empty path" >> pure Nothing 
onTell _ (WarpPkt _ _ (WarpPayload _)) = error "onTell: this function only handles Tell messages (called with WarpPayload)" >> pure Nothing
onTell _ (WarpPkt _ _ (WarpSearch (Ask _))) = error "onTell: this function only handles Tell messages (called with Ask)" >> pure Nothing
onTell ws pkt@(WarpPkt path routesE (WarpSearch (Tell res))) = case routesE of
      -- TELL has been broadcasted
    Left (TTL ttl) -> do
            when (ttl > 0 && isJust res && newPacketP) $ updateState -- a Ressource has been requested, so we update the graph
            if ttl > 1 && newPacketP then do
                info $ "Relaying TELL " ++ show res ++ " on broadcast"
                pure $ Just $ WarpPkt (me:path) (Left $ TTL $ ttl - 1) (WarpSearch (Tell res)) else pure Nothing
      -- TELL follows a route
    Right routeList -> do
            when (isJust res && not (me `elem` path)) $ updateState -- a Ressource has been requested, so we update the graph
            case shouldAcceptP me (_routeTree <$> routeList) of
                Nothing -> pure Nothing -- I'm not on the route
                Just route -> do let newRoutes = R <$> subForest route
                                     relayTell = WarpPkt (me:path) (Right $ newRoutes) $ WarpSearch (Tell res)
                                 if not (null newRoutes)
                                    then do  info $ "Relaying TELL " ++ show res ++ "from path=" ++ show path ++ " to route=" ++ show newRoutes
                                             pure $ Just relayTell
                                 else if null newRoutes
                                     then do info $ "Accepting TELL " ++ show res ++ "from path=" ++ show path
                                             pure Nothing
                                 else pure Nothing


    where updateState :: ADN ()
          updateState   = modifyADNVar_ state $ \resMap -> pure $ resMap & at src . non S.empty  %~ (S.insert $ fromJust res)
          (RessourceState _ state) = _ressourcesState ws
          src = last path
          me = _identity ws
          newPacketP = not $ me `elem` path
            

onAsk :: WarpState -> WarpPkt -> ADN (Maybe WarpPkt)
onAsk ws (WarpPkt path _ (WarpPayload _)) = error "onAsk: this function only handles Ask messages (called with WarpPayload)" >> pure Nothing
onAsk ws pkt@(WarpPkt path _ (WarpSearch _)) = info "Received ASK pkt" >> onAsk' ws pkt

onAsk' ::  WarpState -> WarpPkt -> ADN (Maybe WarpPkt)
onAsk' _ (WarpPkt [] _ _) = error "onAsk: empty path" >> pure Nothing
onAsk' _ (WarpPkt _ _ (WarpSearch (Tell _))) = error "onAsk: this function only handles Ask messages" >> pure Nothing
onAsk' _ (WarpPkt _ (Right _) _)  = error "onAsk: directional research are not handled yet. Please, use broadcast" >> pure Nothing
onAsk' ws (WarpPkt path@(prev:_) (Left ttl) (WarpSearch pkt@(Ask res))) = withADNVar myRessources $ \mines -> case res of
                                                                                            User wid -> onUser wid mines
                                                                                            Res rid ->  onRes rid mines

    where onUser wid mines = if me == wid then pure $ Just $ answerTell Nothing []  -- TODO add the case where we have a route to wid
                                          else if newTTL > 0 && not (me `elem` path) then debug "Relaying ASK [USER] pkt" >> (pure $ Just relayedAsk)  else pure Nothing
          onRes rid mines
            | rid `S.member` mines = do
                        info $ "Sending TELL on: " ++ show path
                        pure $ Just $ answerTell (Just rid) [] 
            | otherwise = withADNVar grM $ \gr -> 
                            withADNVar state $ \resMap -> case [(uid,res) | (uid, res) <- M.assocs resMap, rid `elem` res] of
                                                    [] -> if newTTL > 0 && not (me `elem` path) then pure $ Just relayedAsk  else pure Nothing
                                                    ((uid,res):_) -> do route <- randomSearch_ me gr $ V uid
                                                                        debug $ "found route: " ++ show route
                                                                        if isJust route then pure $ Just $ answerTell (Just rid) $ fromJust route
                                                                                        else error ("onAsk: " ++ show uid ++ " is indexed in the RessourceState, but not in WarpState") >> pure Nothing

                
          newTTL = ttl - 1
          relayedAsk = WarpPkt (me:path) (Left newTTL) $ WarpSearch pkt
          answerTell ressource pathprefix = WarpPkt (me:pathprefix) (Right [pathToRoute path]) $ WarpSearch $ Tell ressource
          (WarpState me _ _ grM (RessourceState myRessources state)) = ws



