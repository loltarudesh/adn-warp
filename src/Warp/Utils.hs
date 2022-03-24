module Warp.Utils where

import Data.List as L
import Data.Word
import Data.IP

readAddr :: String -> IP
readAddr = tupleToIP . extractAddr 

extractAddr :: String -> (Word8, Word8, Word8, Word8)
extractAddr str = toTuple $  fmap read $ splitOn '.' str
    where 
        toTuple [a,b,c,d] = (a,b,c,d)
        splitOn ::  (Eq a) => a -> [a] -> [[a]]
        splitOn el [] = []
        splitOn el l@(x:xs)  
                | x == el = splitOn el xs
                | otherwise = prefix:splitOn el rest
                
            where (prefix, rest) = L.break (== el) l


