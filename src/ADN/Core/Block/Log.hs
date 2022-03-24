module ADN.Core.Block.Log (
  -- * Logging functions
  debug, info, warning, error,
  
  sendLog,
  -- * Custom loggers
  registerLogger
  ) where

import Prelude hiding (error)

import ADN.Core.Block.Tools
import ADN.Core.Block.ADN


-- |log a Debug level message
debug   = sendLog . Log Debug []    
-- |log a Info level message
info    = sendLog . Log Info []    
-- |log a Warning level message
warning = sendLog . Log Warning []  
-- |log a Error level message
error   = sendLog . Log Error []    


sendLog :: Log -> ADN ()
sendLog l = do logger <- cmdLog <$> readBlockCmd
               logger l 

-- | Set the function called by log from this block.
registerLogger :: (Log -> ADN ()) -> ADN ()
registerLogger l = modifyCmd $ \c -> c{cmdLog = l}


