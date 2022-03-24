{-# LANGUAGE TemplateHaskell #-}

{-|
    This module defines a (non oriented) graph in which Warp will store its knowledge about the current state of the network.

    - Nodes represent the devices.
    - Two nodes are connected if there exists a direct link between them.
    - Edges timeout at some point if they are inactive for too long, i.e. no packets that have taken this edge.
    - Isolated nodes are discarded.

    The graph keeps track of the ressources proposed by each node. These informations are stored into the RessourceState datatype.

-}



module Warp.Types.States where

import Control.Lens
import Control.Concurrent.Suspend
import ADN.Core
import Warp.Types.Identifiers
import Warp.Types.Graph
import Data.Map as M
import Data.Set


data WarpState = WarpState {
                    _identity :: WID, -- ^ The user identifier of this warp daemon
                    _initialTTL :: TTL,
                    _edgeTimeout :: Delay, -- ^ The maximum inactivity duration of an edge
                    _graph :: ADNVar Graph, -- ^ A pointer to the graph representation
                    _ressourcesState :: RessourceState -- ^ The ressource associated to each node
                 }



data RessourceState = RessourceState { 
                        _myRessources :: ADNVar (Set RID), -- ^ The ressources provided by this warp daemon
                        _verticesRessources :: ADNVar (M.Map WID (Set RID)) -- ^ The ressources provided by some nodes. This map is synchornized with the graph, and entries will timeout at the same time of the vertice they are associated to.
                    }    
makeLenses ''WarpState 
makeLenses ''RessourceState


