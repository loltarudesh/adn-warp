{-# LANGUAGE DeriveGeneric #-}

module Warp.Routing.Route ( shouldAcceptP, pathToRoute ) where

import Data.Tree
import Warp.Types




-- Returns the subtree where the userid belongs to
shouldAcceptP :: WID -> [Tree WID] -> Maybe (Tree WID)
uID `shouldAcceptP` routes = safeHead myroutes
    where myroutes = [subtree | subtree <- routes, uID == rootLabel subtree]



         

pathToRoute :: [WID] -> Route
pathToRoute path = R $ foldr1 (\e a -> e{subForest = [a]}) (fmap (\uid -> Node uid []) path)
