{-# LANGUAGE DeriveGeneric #-}
module Data.IP (
  -- * IP addresses
  IP(..), ipToTuple, tupleToIP,

  -- * Pools of IP addresses
  IPPool, emptyIPPool, allocIP, freeIP, lookupIP, setIP
  ) where

import Data.IP.Addr
import Data.IP.Pool

