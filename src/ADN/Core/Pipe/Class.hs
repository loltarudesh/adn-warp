{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module ADN.Core.Pipe.Class(
  -- * Channels
  Sink(..), Source(..), Channel(..),

  -- * PipeStart
  PipeStart(..), PipeStartBuilder(..),

  -- * Pipe
  Pipe(..), PipeBuilder(..),

  -- * PipeEnd
  PipeEnd(..), PipeEndBuilder(..)) where


import ADN.Core.Block


-- | A Sink is a packet-processing function, similar to a network socket /send/ function.
-- it shoud return quickly. 
type Sink p = p -> ADN ()

-- | a Source is a IO function producing packets, similar to a network socket /recv/function.
-- it should blocks until contents is available.
type Source p = ADN p

-- | A Channel is a 2-way communication medium. 
-- it consists of a Sink and a Source
data Channel p = Channel { send :: Sink p,  -- ^ sending function: should return immediately
                           recv :: Source p -- ^ blocking function, return the next piece of data.
}


-- |a PipeStart is the begining of a pipeline: it produces a Channel for the upper layer
type PipeStart p = ADN (Channel p)
-- |The class PipeStartBuilder is implemented by every module acting like the beginning of a pipeline (for example: a link to the physical layer)
class PipeStartBuilder a p | a -> p where pipeStart :: a -> PipeStart p
instance PipeStartBuilder (PipeStart p) p where pipeStart f = f

-- |Pipe can be chained in a pipeline: it takes as argument a Channel to the lower layer,
-- and produces one for the upper layer.
type Pipe p p' = Channel p -> ADN (Channel p')
-- |The class PipeBuilder is implemented by every module acting on a (bidirectionnal) stream of packet: communicating with both a lower and an upper layers.
class PipeBuilder a p p' | a -> p, a -> p' where pipe :: a -> Pipe p p'
instance PipeBuilder (Pipe p p') p p' where pipe f = f

-- | a PipeEndBuilder is the end of a pipeline: it takes as argument a Channel to the lower layer.
type PipeEnd p = Channel p -> ADN ()
-- |The class PipeStartBuilder is implemented by every module acting like the end of a pipeline (for example: a link to the applicative layer)
class PipeEndBuilder a p | a -> p where pipeEnd :: a -> PipeEnd p
instance PipeEndBuilder (PipeEnd p) p where pipeEnd f = f


