module Merge where

import Data.Map.Strict (Map)
import qualified Data.Map.Merge.Strict as Merge

-- A slightly nicer API for merging maps
data MergeMaps l r = LeftOnly l
                   | Both l r
                   | RightOnly r

mergeMaps :: Ord k => (MergeMaps l r -> a) -> Map k l -> Map k r -> Map k a
mergeMaps f = Merge.merge (Merge.mapMissing (\_ l -> f (LeftOnly l)))
                          (Merge.mapMissing (\_ r -> f (RightOnly r)))
                          (Merge.zipWithMatched (\_ l r -> f (Both l r)))
