{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


{-| 
    This module defines basic tools for working with edge valued graphs.
    This representation stores the graph as a hashmap associating each vertice to its neighborhood.
    A neighborhood associates each neighbor to the edge datatype
-}

module Warp.Types.Graph (
         Route(..)
        ,Vertice(..)
        ,Edge(..)
        ,Neighbors(..)
        ,Graph(..)
        ,adjMap)
 where

import Warp.Types.Identifiers

import GHC.Generics
import Data.Binary
import Data.Tree
import qualified Data.Map as M
import Control.Concurrent.Timer
import Control.Lens


newtype Route = R {_routeTree :: Tree WID} -- ^ A route is described using a tree to allow native multicast
    deriving (Generic)


data Vertice = V { _uid :: WID}
data Edge = E {
                    _extremities :: (Vertice, Vertice),
                    _timer :: TimerIO
              }
type Neighbors = M.Map Vertice Edge
data Graph = G {_adjMap :: M.Map Vertice Neighbors} -- ^ A graph is map between vertices and their neighborhood. A neighbohood is a map, bounding the neighobor's userID to the corresponding edge label

instance Binary Route
instance Show Vertice where
    show (V ( WID x)) = "V#" ++ show x
instance Eq Vertice where
    v == v' = _uid v == _uid v'
instance Ord Vertice where
    v `compare` v' = _uid v `compare` _uid v'

instance Show Edge where
    show (E vs _) = ""
instance Eq Edge where
    e == e' = _extremities e == _extremities e'
instance Ord Edge where
    e `compare` e' = _extremities e `compare` _extremities e'


instance Show Route where
    show (R tr) = show' tr
        where show' (Node x l) = "[" ++ show x ++ unwords (fmap show' l) ++ "]"
instance Show Graph where
    show (G gr) = show' gr
        where show' gr = unlines $ show'' <$> M.assocs gr
              show'' (k,v) = show k ++ "\t => \t" ++  (show $ M.keys v)




makeLenses ''Vertice
makeLenses ''Edge
makeLenses ''Graph

