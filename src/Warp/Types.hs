{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Warp.Types (
  -- * Users
  WID (..), RID (..),

  -- * Routes
  Route(..), safeHead,
  TTL (..),

  -- * Graphs
  Vertice(..), Edge(..), Graph(..),
  adjMap,
  Neighbors,

  -- * Warp
  WarpState(..), WarpMsg(..),
  WarpPkt(..), path, route,
  WarpContents(..),
  WProtocol(..),

  -- * Ressources
  RessourceState (..), Ressource(..),
  SearchPkt (..)


  ) where

import Warp.Types.Identifiers
import Warp.Types.Graph
import Warp.Types.Packets
import Warp.Types.States




-- utils
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l = Just $ head l


