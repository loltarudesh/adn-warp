module ADN.Core.Block.Tools(
  -- * Blocks
  getBlock,
  runBlock,
  lookupSubBlock,
  deleteSubBlock,
  readADNConf,
  readBlockCmd,
  readBlockSubs,

  -- * Utilities
  makeLift,
  modifyCmd,
  modifySubs,
  clearBlock,
  emptyBlock,
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent.MVar
import qualified Data.Map as M

import ADN.Core.Block.ADN


-- | generate a lift function, allowing to execute ADN action inside any IO monad, using the caller's block 
makeLift :: MonadIO m => ADN (ADN a -> m a)
makeLift = do
    b <- getBlock
    return $ runBlock b

-- | Return the current Block
getBlock :: ADN Block
getBlock = ADN ask

-- | Run a ADN action with a given Block
runBlock :: MonadIO m => Block -> ADN a -> m a
runBlock b (ADN p) = liftIO $ runReaderT p b

-- | retreive a sub-block, if it exist
lookupSubBlock :: BlockName -> ADN (Maybe Block)
lookupSubBlock n = M.lookup n <$> readBlockSubs

-- | delete a sub-block
deleteSubBlock :: BlockName -> ADN ()
deleteSubBlock name = modifySubs $ M.delete name


-- ##### INTERNAL #####

modifyCmd :: (BlockCmd -> BlockCmd) -> ADN ()
modifyCmd f = do
    cmd <- blkCmd <$> getBlock
    liftIO $ modifyMVar_ cmd (pure . f)

modifySubs :: (SubBlocksMap -> SubBlocksMap) -> ADN ()
modifySubs f = do
    subs <- blkSubs <$> getBlock
    liftIO $ modifyMVar_ subs (pure . f)

readBlockSubs :: ADN SubBlocksMap
readBlockSubs = (blkSubs <$> getBlock) >>= liftIO . readMVar

readBlockCmd :: ADN BlockCmd 
readBlockCmd = (blkCmd <$> getBlock) >>= liftIO . readMVar

readADNConf :: ADN ADNConf
readADNConf = blkConf <$> getBlock

clearBlock = do
    modifyCmd $ pure emptyBlockCmd
    modifySubs $ pure emptyBlockSubs

emptyBlock :: MonadIO m => ADNConf -> m Block
emptyBlock conf = Block conf <$> newCmd <*> newSubs
    where 
        newCmd = liftIO $ newMVar emptyBlockCmd
        newSubs = liftIO $ newMVar emptyBlockSubs
    
emptyBlockSubs = M.empty :: SubBlocksMap
emptyBlockCmd = BlockCmd M.empty (pure ()) (pure ()) (pure $ pure ())


