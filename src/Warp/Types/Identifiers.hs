{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| This module introduces defines the identifiers used by Warp:

        * /Users/ are identified by a WarpID (WID) - not necessarily unique across the network
        * /Ressources/ are identified by a RessourceID (RID). Users will request route to some ressources. This identifier be unique
        * /Protocols/ are identified by a ProtocolID (WProtocol). When a packet arises from the network, Warp will use this identifier to select the correct upper layer. This identifier must be unique 
-}

module Warp.Types.Identifiers where



import GHC.Generics
import Data.Binary

-- | A basic wrapper for a time to live value (in seconds)
newtype TTL = TTL Int -- TODO this is not an identifier...
    deriving (Eq, Ord, Show, Num, Generic)
instance Binary TTL


-- | Identifier of a user on the network (only needs to be unique among neighbours)
newtype WID= WID {getID :: Int}
    deriving (Generic, Eq, Ord)

-- | a unique identifier for a Ressource
newtype RID = RID Int
    deriving (Eq, Ord, Generic)

-- | a unique identifier for each independent service registered on Warp
newtype WProtocol = WProtocol Int
    deriving (Eq, Ord, Generic, Show)


instance Show RID where show (RID s) = "R#" ++ show s
instance Binary RID

instance Binary WID 
instance Show WID where
    show (WID u) = "U#" ++ show u

