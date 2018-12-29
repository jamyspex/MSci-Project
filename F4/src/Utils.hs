module Utils where

import qualified Data.Map as DMap

removeDuplicates :: Ord a => (b -> a) -> [b] -> [b]
removeDuplicates getKey input = DMap.elems uniqueMap
    where
        pairsForMap = map (\item -> (getKey item, item)) input
        uniqueMap = foldr addToMap DMap.empty pairsForMap
        addToMap :: Ord a => (a, b) -> DMap.Map a b -> DMap.Map a b
        addToMap (key, val) map = DMap.insert key val map
