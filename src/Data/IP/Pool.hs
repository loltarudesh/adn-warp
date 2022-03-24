module Data.IP.Pool (
  -- * Pools of IP addresses
  IPPool, emptyIPPool, allocIP, freeIP, lookupIP, setIP
  ) where


import Data.List
import Data.Word
import Data.Bits
import qualified Data.Array as A

import Data.IP.Addr


data IntervalPool a = IntervalPool {
  valArray :: A.Array Int (Int,a),
  indArray :: A.Array Int Int,
  poolSize :: Int
  }

instance Show a => Show (IntervalPool a) where
  show (IntervalPool vals _ size) = "[" ++ intercalate "," [show (vals A.! i) | i <- A.range (0,size-1)] ++ "]"


emptyIntervalPool :: Int -> IntervalPool a
emptyIntervalPool capacity = IntervalPool initVals initInds 0
  where initVals = A.array (0,capacity-1) [(i,(i,undefined)) | i <- A.range (0,capacity-1)]
        initInds = A.array (0,capacity-1) [(i,0) | i <- A.range (0,capacity-1)]

poolHas :: IntervalPool a -> Int -> Bool
poolHas (IntervalPool vals inds size) i = ii < size && fst (vals A.! ii) == i
  where ii = inds A.! i

poolLookup :: IntervalPool a -> Int -> Maybe a
poolLookup pool@(IntervalPool vals inds size) i | poolHas pool i = Just (snd (vals A.! (inds A.! i)))
                                                | otherwise = Nothing

poolSet :: IntervalPool a -> Int -> a -> IntervalPool a
poolSet pool@(IntervalPool vals inds size) i x | poolHas pool i = IntervalPool newvals inds size
                                               | otherwise = pool
  where newvals = vals A.// [(inds A.! i, (i, x))]

poolAlloc :: IntervalPool a -> a -> (Int,IntervalPool a)
poolAlloc (IntervalPool vals inds size) x = (res,IntervalPool newvals newinds (size+1))
  where res = fst (vals A.! size)
        newinds = inds A.// [(res,size)]
        newvals = vals A.// [(size,(res,x))]
        
poolFree :: IntervalPool a -> Int -> IntervalPool a
poolFree pool@(IntervalPool vals inds size) i | poolHas pool i = IntervalPool newvals newinds (size-1)
                                              | otherwise = pool
  where ii = inds A.! i
        ~(last_vi,last_v) = vals A.! (size-1)
        ~(cur_vi,cur_v) = vals A.! ii
        newvals = vals A.// [(size-1,(cur_vi,cur_v)) , (ii,(last_vi,last_v))]
        newinds = inds A.// [(cur_vi, size-1) , (last_vi, ii)]

-- | An IP Pool, that can efficiently allocate and free IP addresses
-- in a range, and associate some data to those addresses.
data IPPool a = IPPool {
  base :: IP,
  offsets :: IntervalPool a
  }

-- | Create an empty IP Pool
emptyIPPool :: IP -- ^ A base address
            -> IP -- ^ The last address of the available range
            -> IPPool a
emptyIPPool base@(IP start) (IP end) = IPPool base (emptyIntervalPool (fromIntegral (end - start + 1)))

-- | Allocate a new IP from a pool (and associate that IP with a value)
allocIP :: IPPool a -- ^ The pool
        -> a        -- ^ A value to associate with the newly-allocated IP
        -> (IP, IPPool a)
allocIP (IPPool base@(IP ip) offsets) x = (IP (ip + fromIntegral off), IPPool base offsets')
  where (off, offsets') = poolAlloc offsets x

-- | Free a previously allocated IP
freeIP :: IPPool a -> IP -> IPPool a
freeIP (IPPool base@(IP x) offsets) (IP y) = IPPool base (poolFree offsets (fromIntegral (y-x)))

-- | Look a value up from its IP
lookupIP :: IPPool a -> IP -> Maybe a
lookupIP (IPPool (IP x) offsets) (IP y) 
    | n < poolSize offsets = poolLookup offsets n
    | otherwise = Nothing
        where n = fromIntegral $ y - x

-- | Set the value associated with an IP (that must have been allocated before)
setIP :: IPPool a -> IP -> a -> IPPool a
setIP (IPPool base@(IP x) offsets) (IP y) v = IPPool base (poolSet offsets (fromIntegral $ y-x) v)

{- Not sure if this is needed or not... Maybe little more control than deriving!
instance Binary IP where
    put (IP ip) = put $ tupleToHostAddress ip
    get = IP . hostAddressToTuple <$> get
-}

