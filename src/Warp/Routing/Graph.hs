{-# LANGUAGE TemplateHaskell #-}
module Warp.Routing.Graph(  
  insertPath, 
  randomSearch_,
  randomSearch)
  where

import qualified Data.Map as M
import Control.Lens
import Warp.Types
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import Control.Monad.Trans
import Control.Monad
import Data.Maybe
import Data.List
import ADN.Core
import Prelude hiding (error)

--edgeTimeout = sDelay 10 -- 10s timeout
--infty = 10000 -- maximum len -- where is this used ??!!!



isExtremityOf :: Vertice -> Edge -> Bool
isExtremityOf v edge = v == v1 || v == v2
    where  (v1, v2) = _extremities edge


deleteEdge :: WarpState -> Vertice -> Vertice -> ADN ()
deleteEdge ws  v v' = modifyADNVar_ (_graph ws) $ \gr -> 
                      modifyADNVar rs $ \resMap -> do
                                let newgraph :: Graph
                                    newgraph_ = gr & adjMap . at v . _Just . at v' .~ Nothing  -- delete (v,v')
                                                   & adjMap . at v' . _Just . at v .~ Nothing  -- delete (v',v)
                                                     -- & adjMap %~ M.filterWithKey (\k v -> k == V (_identity ws) || v /= M.empty) -- delete empty vertices
                                    (newAdj, removedMap) = M.partitionWithKey  (\k v -> k == V (_identity ws) || v /= M.empty) $ _adjMap newgraph_ 
                                    newgraph = newgraph_ & adjMap .~ newAdj
                                    newResMap = foldr (\(V e) a -> M.delete e a) resMap (M.keys removedMap)
                                pure (newResMap, newgraph)
    where (RessourceState _ rs) = _ressourcesState ws

{-| Inserts the edge (v,v') into the graph, or refresh its timer -} 
insertEdge :: WarpState -> Vertice -> Vertice -> ADN ()
insertEdge ws v v' = modifyADNVar_ (_graph ws) $ \gr -> case curEdge gr of 
                                                            Nothing -> do
                                                                        lift <- makeLift
                                                                        timer <- liftIO $ oneShotTimer (lift $ deleteEdge ws v v') $ _edgeTimeout ws
                                                                        pure $ addEdge gr (edge timer)-- Since the graph is assumed to be symetric, there is no need to search for the edge (v',v)
                                                            Just e -> liftIO (oneShotRestart (_timer e)) >> pure gr -- For the same reason, it is enough to refresh the timer of the *directed* edge (v,v')

    where edge timer = E (v,v') timer
          addEdge :: Graph -> Edge -> Graph
          addEdge gr e = gr & adjMap . at v . non M.empty . at v' ?~ e
                            & adjMap . at v' . non M.empty . at v ?~ e

          curEdge :: Graph -> Maybe Edge  
          curEdge gr =  (gr ^. adjMap . at v) >>= (^. at v') -- if done directly, the compiler asks for a semigroup instance for Edge
          rs = _ressourcesState ws
                    

{-| Inserts a path of WID into the graph -}
insertPath :: WarpState -> [WID] -> ADN ()
insertPath ws [] = error "trying to insert an empty path"
insertPath ws [v'] = pure ()
insertPath ws (v:v':vs) = insertEdge ws (V v) (V v') >> insertPath ws (v':vs)


{-| Looks for a route between two nodes into the graph. 
    Note: this has been implemented currently as a DFS, but it will evolve as BFS asap. -}
randomSearch :: WarpState -> Vertice -> ADN (Maybe [WID])
randomSearch ws dest = do 
        gr <- readADNVar (_graph ws) 
        randomSearch_ me gr dest
    where me = _identity ws 
randomSearch_ :: WID -> Graph -> Vertice -> ADN (Maybe [WID])
randomSearch_ me gr dest = do 
                            route <- randomSearch' gr [V me] $ V me
                            pure (fmap (tail . map (_uid) . reverse) route)

    where   
            randomSearch' :: Graph -> [Vertice] -> Vertice -> ADN (Maybe [Vertice])
            randomSearch' gr route cur 
                    | cur == dest = pure $ Just route
                    | otherwise = case fmap ((\\ route).M.keys)  $ M.lookup cur (_adjMap gr) of
                                    Nothing -> error ("Looking for unknown userid:" ++ show cur) >> pure Nothing
                                    Just [] -> pure Nothing -- All neighbors have been explored
                                    Just neigh -> do
                                               rec <- forM neigh $ \v' -> randomSearch' gr (v':route) v' 
                                               pure $ safeHead $ catMaybes rec




