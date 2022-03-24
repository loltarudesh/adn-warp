{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ADN.Core.Block.ADN (
  -- * The ADN Monad
  ADN(..),ADNConf(..),

  -- * Blocks
  Block(..), BlockName(..), SubBlocksMap(..),

  -- * Commands
  CLI(..), Order(..),
  BlockCmd(..), -- cmdCLI, cmdClose, cmdCloseAfter, cmdLog,

  -- * Logging
  LogLevel(..), Log(..),

  -- * Misc
  ThreadID(..)
  ) where

import Control.Monad.IO.Class
import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Catch

import ADN.Core.Conf

-- | This is the Monad used to build and modify a Block of ADN:
-- it is notably used to register closing callbacks,
-- allowing to name parts of the pipeline ("block"), and address
-- them (and close them, for example) independently
newtype ADN a = ADN (ReaderT Block IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | A Block is a mutable BlockCnt, shared by every thread of the
-- block, allowing them to read and modify it.
data Block = Block {
    blkConf :: ADNConf,             -- ^ global static configuration for ADN
    blkCmd :: MVar BlockCmd,        -- ^ handles for this block functionnalities: logging, closing, CLI
    blkSubs :: MVar SubBlocksMap    -- ^ map of sub-blocks
    }
type SubBlocksMap = M.Map BlockName Block
type BlockName = String


-- | Cmd of the current block, allowing to send instructions, retrieve response, and close the block.
data BlockCmd = BlockCmd {
    cmdCLI :: CLI,              -- ^ Orders recognized by this block
    cmdCloseBefore :: ADN (),   -- ^ closing callback, called before closing the sub-blocks
    cmdCloseAfter :: ADN (),    -- ^ closing callback, called after closing the sub-blocks
    cmdLog :: Log -> ADN ()      -- ^ logging function
}

type ThreadID = ThreadId      --maybe it's just me...

data LogLevel = Debug | Info | Warning | Error 
    deriving (Eq, Ord, Show, Enum)
data Log = Log
  LogLevel     -- ^ A log level
  [BlockName]  -- ^ A list of blocknames, denoting the full path from which the message originates
  String       -- ^ The log message per se
    deriving (Eq, Ord)
instance Show Log where
    show (Log l d m) = "[" ++ show l ++ "]\t" ++ loc ++ " \t -> " ++ m
        where loc | null d = "/" | otherwise = concat $ map ("/"++) d


-- | A command handler, used in the command-line interface to provide specific behaviour to a block.
-- 
-- If the user types in a command of the form "<cmd> <rest>", the '<rest>' is passed as an argument to the
-- corresponding handler, and the handler should return a String that is printed out
--type CLI = String -> ADN String
type CLI = M.Map String Order
data Order = Order {
    orderDescription :: String,
    orderCall :: String -> ADN String
}



