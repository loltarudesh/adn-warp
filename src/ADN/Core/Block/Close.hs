module ADN.Core.Block.Close (
  registerClose,
  registerCloseAfter,
  closeThisBlock,
  closeBlock
) where

import Control.Monad
import Control.Monad.IO.Class

import ADN.Core.Block.Tools
import ADN.Core.Block.ADN
import ADN.Core.Block.Log


-- | Close the current block
closeThisBlock :: ADN ()
closeThisBlock = getBlock >>= closeBlock

-- | close a given block, by calling close callbacks. Also closes every sub-blocks.
closeBlock :: MonadIO m =>  Block -> m ()
closeBlock b = runBlock b $ do
    debug "closing block"
    cmd <- readBlockCmd
    subs <- readBlockSubs
    cmdCloseBefore cmd 
    forM_ subs closeBlock
    cmdCloseAfter cmd
    clearBlock


-- | Register a closing callback in the ADN monad.
-- it is useful for linking channels together, without worrying about closing the threads.
registerClose :: ADN () -> ADN ()
registerClose c = modifyCmd regC
    where regC cmd = cmd{cmdCloseBefore = c >> cmdCloseBefore cmd } 

-- | Register a close callback executed AFTER closing the sub-block
registerCloseAfter :: ADN () -> ADN ()
registerCloseAfter c = modifyCmd regC
    where regC cmd = cmd{cmdCloseAfter = cmdCloseAfter cmd >> c} 



