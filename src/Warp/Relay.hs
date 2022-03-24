module Warp.Relay ( relayPacket ) where

import ADN.Core
import Warp.Packet
import Warp.Routing.Route
import Warp.Types
import Data.List
import Data.Tree
import Data.Maybe
import Control.Monad
import Control.Lens

relayPacket :: WarpState -> Sink WarpPkt -> Sink WarpPkt -> WarpPkt -> ADN ()
relayPacket ws toLower toUpper pkt = case _route pkt of
                                            Left ttl -> when (ttl > 0) $ toUpper pkt -- broadcast packet
                                            Right dirRoute -> let -- Directionnal route
                                                       ourRoute = catMaybes [shouldAcceptP (_identity ws) (_routeTree <$> dirRoute)]
                                                       (pshRoute, relayRoute) = partition (\route -> null $ subForest route) ourRoute
                                                       relayPkt :: [WarpPkt]
                                                       relayPkt = fmap (\r -> pkt & route .~ Right (R <$> subForest r) & path %~ (_identity ws:) ) relayRoute
                                                     in do 
                                                           forM_ relayPkt (\pkt -> debug ("Relaying packet: " ++ show pkt) >> toLower pkt) 
                                                           forM_ pshRoute (pure $ toUpper pkt)


