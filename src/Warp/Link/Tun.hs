{-# LANGUAGE MultiParamTypeClasses #-}
-- | Link to a virtual network interface
module Warp.Link.Tun (
  -- * Pipes
  ADNTun(..),
  ) where

import Prelude hiding (error)
import Control.Monad.IO.Class
import System.Posix.Types
import System.Posix.IO
import System.IO

import ADN.Core
import ADN.Handle
import Data.IP.Packet


-- |a virtual network interface, created by ADN to route IP traffic.
--
-- the interface creation is actually done outside the program, for security reasons.
-- This module simply retreive a file descriptor 'Fd' from the arguments, wrap it into a 'ADNHandle'. 
data ADNTun = ADNTun {
    tunName :: String,      -- ^ the name of the interface.
--    tunFd :: Fd,            -- ^ file descriptor transmitted by the priviledged layer (launcher in C)
    tunMTU :: Int          -- ^ maximum size of packets  
}


-- | a link to a virtual network interface
instance PipeEndBuilder ADNTun IPPkt where
    pipeEnd tun@(ADNTun name mtu) c = block name $ do
        h <- createTunInterface tun
        block "handle"  $ pure c >>= decoder >>= pipeEnd (ADNHandle mtu h)

-- | a link to a virtual network interface
instance PipeStartBuilder ADNTun IPPkt where
    pipeStart tun@(ADNTun name mtu) = block name $ do
      h <- createTunInterface tun
      block "handle" $ pipeStart (ADNHandle mtu h) >>= decoder


-- ##### INTERNAL #####

{-| Example: createTunInterface "adn0" "10.10.0.1" "10.10.0.0/24" -- route the trafic from 10.10.0/24 into adn0 -}

createTunInterface :: ADNTun -> ADN Handle
createTunInterface tun@(ADNTun name _) = do
        debug $ "creating tun interface: " ++ name
        fdM <- getTunFD name
        case fdM of
            Just fd -> liftIO . fdToHandle . Fd $ toEnum fd
            Nothing -> do
                error $ "unable to retreive file descriptor for interface : " ++ name
                liftIO $ fdToHandle (-1)

{-
 -
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO

foreign import ccall "tuntap.c create_tun"  ffi_create_tun :: CString -> IO CInt

createTunInterface :: ADNTun -> ADN Handle
createTunInterface tun@(ADNTun name _ _ ) = do 
        debug $ "creating tun interface: " ++ name
        h <- liftIO $ withCString name (fmap Fd . ffi_create_tun) >>= fdToHandle
        pure h
-}
