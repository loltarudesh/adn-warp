module ADN.Core.Block.Run (
  -- * Creating Blocks
  block,
  forkADN,
  forkBlock,
  maybeInSubBlock,
  insertSubBlock,

  -- * Managing Blocks
  closeSubBlock,

  -- * Running
  runADN,
  ) where

import Prelude hiding(error)
import Control.Lens
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.Map as M


import ADN.Core.Block.ADN
import ADN.Core.Block.Tools
import ADN.Core.Block.Close
import ADN.Core.Block.Log
import ADN.Core.Block.CLI

-- | Create a new nameless block and runs the specified action inside a MonadIO.
--  User is responsible for calling closeThisBlock at the end of the execution.
-- intended for top-level use only
runADN :: MonadIO m => ADNConf -> ADN () -> m ()
runADN conf p = do
    b <- newBlock conf
    runBlock b ( p >> closeThisBlock) 

-- | Creates a new block of execution in the monad
-- The action given in argument become a sub-block of the caller's block, identified by a name.
-- if a block with the same name already exist, execute the action within the existing block
block :: BlockName -> ADN a -> ADN a 
block name p | null name = warning "no name specfied for block, running in caller's block" >> p
             | otherwise = maybeInSubBlock name createBlock alreadyExist 
    where
        --alreadyExist = debug "A block with this name already exist" >> p
        alreadyExist = p
        createBlock = do
            debug $ "creating new sub-block: " ++ name
            b <- readADNConf >>= newBlock 
            insertSubBlock name b
            runBlock b p

-- | like a forkIO, but the thread is killed when the block is closed (after the sub-blocks)
forkADN :: ADN () -> ADN ()
forkADN act = do
    l <- makeLift
    pID <- liftIO $ forkIO (l act) 
    debug $ "creating thread " ++ show pID
    registerClose $ do
        debug $ "killing thread " ++ show pID
        liftIO $ killThread pID

-- | Like forkADN, but encapsulating it in a new block, allowing to close it gracefully afterward
forkBlock :: BlockName -> ADN () -> ADN ()
forkBlock name = block name . forkADN


-- | Close a sub-block, specifying it's name
closeSubBlock :: BlockName -> ADN ()
closeSubBlock name = maybeInSubBlock name logError closeThisBlock 
    where logError = error $ "attempted to close unknown sub-block: " ++ name

-- | If the sub-block exist, run the second action within it. Otherwise, run the first one, in the caller's block.
maybeInSubBlock :: BlockName -> ADN a -> ADN a -> ADN a
maybeInSubBlock name ifNot ifYes = lookupSubBlock name >>= maybe ifNot runSub 
    where runSub b = runBlock b ifYes

-- #### INTERNAL ####

newBlock :: MonadIO m => ADNConf -> m Block 
newBlock conf = do 
    b <- emptyBlock conf
    runBlock b $ registerBlockCLI
    return b

-- | insert a block as a sub-clock 
insertSubBlock :: BlockName -> Block -> ADN ()
insertSubBlock name sub = do 
    bM <- lookupSubBlock name
    case bM of
        Just _ -> error $ "A sub-block is already registered with this name: " ++ name
        Nothing -> do
            liftADN <- makeLift 
            modifySubs $ M.insert name sub
            let subLog (Log l d m) = liftADN . sendLog $ Log l (name:d) m
            runBlock sub $ do
                    registerCloseAfter . liftADN $ deleteSubBlock name
                    runBlock sub $ registerLogger subLog



