{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Warp.Types.Packets where

import ADN.Core
import Warp.Types.Identifiers
import Warp.Types.Graph

import GHC.Generics
import Data.Binary
import Control.Lens

data Ressource = User WID 
               | Res RID 
    deriving (Eq, Ord, Generic)



{-| A WarpPkt is designed to interract with the lower layer -}
data WarpPkt = WarpPkt {
                   _path :: [WID], -- ^ The nodes taken by the packet
                   _route :: Either TTL [Route],  -- ^ The remaining route or a broadcast TTL
                   _payload :: WarpContents -- ^ The content of the packet
               }
            deriving Generic

-- | A WarpPkt can be either a research (managed internally) or a binary payload (pushed to the upper layer)
data WarpContents = WarpSearch SearchPkt | WarpPayload Payload
    deriving Generic

{-| A WarpMsg is designed to interract with the upper layer. Note that the SearchPkt are not propagated to the upper layer -}
data WarpMsg = WarpMsg {
    _wmDest :: WID,
    _wmPayload :: Payload
}
    deriving Generic


{-| A SearchPkt to discover new routes.  -}
data SearchPkt = Ask {askRes :: Ressource }
               | Tell {tellRes :: Maybe RID } -- Nothing if a user has been searched
    deriving (Show,Generic)







-- Instances

instance Binary WarpPkt 
instance Binary SearchPkt
instance Binary WarpContents

instance Binary WarpMsg 
instance Binary WProtocol

instance Show Ressource where 
    show (User wid) = show wid
    show (Res rid) = show rid
instance Binary Ressource


makeLenses ''WarpPkt
