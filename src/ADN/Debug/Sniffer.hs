{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ADN.Debug.Sniffer (
  -- * Debugger pipes
  Sniffer(..), sniffer
) where

import ADN.Core


-- | A Sniffer is a PipeBuilder which simply log the packets transitting through it. 
data Sniffer p = Sniffer {
    snifferName :: String,
    snifferShow :: p -> String
}



-- | log the packets going through it
instance PipeBuilder (Sniffer p) p p where
    pipe (Sniffer n s) (Channel si so) = block n $ do
        liftADN <- makeLift
        let si' p = snif "up -> low" p >> si p
            so' = do p <- so
                     snif "low -> up" p
                     return p
            snif m p = liftADN $ info $ "packet received " ++ m ++ " : " ++ s p
        return $ Channel si' so'

-- | make a 'Sniffer' using the 'Show' instance
sniffer :: Show p => String -> Pipe p p
sniffer name = pipe $ Sniffer name show



