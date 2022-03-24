{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module ADN.UNIXStream where
{-
 (
        UNIXStreamConf (..),
        Port (..),
        UNIXStream,
        makeUnixStream,
        addNewClient,
        defaultUNIXStreamConf
    ) where


import Control.Monad 
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent 
import Control.Concurrent.MVar 
import Control.Concurrent.Chan 
import Network.Socket
import qualified Network.Socket.ByteString as S
import qualified Data.Map as M
import qualified Data.ByteString as BS

import ADN.Core as ADN
import ADN.Dispatcher


defaultUNIXStreamConf = UNIXStreamConf
                                 1024        -- Maximum packet length (bytes)
                                    5        -- Maximum number of waiting connection 
        "./sockets/ADN_UNIX_server.sock"     -- path to the server socket-file.


data UNIXStreamConf = UNIXStreamConf {
        aloPacketMaxLength :: Int,
        aloQueuelength :: Int,
        aloSocketname :: String
}

data UNIXStream = UNIXStream {
    conf :: UNIXStreamConf,
    dispatcher :: Dispatcher Int Raw Raw,
    control :: Pipe Raw Raw
}
instance PipeEndBuilder UNIXStream Raw where
    pipeEnd state c = do
        forkADN $ runServer state
        pipeEnd (dispatcher state) c

type Port = Int


makeUnixStream :: PipeBuilder a Raw Raw => UNIXStreamConf -> a -> ADN UNIXStream
makeUnixStream opt p = do
    dsp <- byteDispatcher 
    return $ UNIXStream opt dsp (pipe p)


addNewClient :: UNIXStream -> Port ->  Socket -> ADN ()
addNewClient state id sock = do
    let name = show id
    block name $ do
        c1 <- sock2Chan (conf state) sock
        c2 <- openPort state id
        linkChannels c1 c2


openPort :: UNIXStream -> Port -> ADN (Channel Raw)
openPort state id = pipeStart protocol >>= control state
        where protocol = Protocol (dispatcher state) id 


sock2Chan :: UNIXStreamConf -> Socket -> ADN (Channel Raw)
sock2Chan opt s = do 
    registerClose . liftIO $ close s
    lf <- makeLift 
    return $ Channel sendS $ liftIO (recvS lf)
        where sendS = liftIO . S.sendAll s . _rawCnt
              recvS lf = do d <- S.recv s (aloPacketMaxLength opt)
                            if BS.null d then lf closeThisBlock else pure () -- TODO ! Return without sending packet
                            pure $ ADN.Raw d
                        

runServer ::  UNIXStream -> ADN ()
runServer state = do
    serverSock <- startServer $ conf state
    registerCloseAfter . liftIO $ close serverSock
    forever $ do
        (sock,_) <- liftIO $ accept serverSock
        addNewClient state 0 sock -- TODO!! Gen free ID


startServer :: (MonadIO m) => UNIXStreamConf -> m Socket
startServer opt = liftIO $ do
        sock <- socket AF_UNIX Stream defaultProtocol       -- Creating the server UNIX Socket
        setSocketOption sock ReuseAddr 1                    -- why isn't this default?!!
        bind sock . SockAddrUnix $ aloSocketname opt        -- bind it to the socket file
        listen sock $ aloQueuelength opt                    -- listen to connections
        return sock


-}


