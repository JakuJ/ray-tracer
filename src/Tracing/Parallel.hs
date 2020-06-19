module Tracing.Parallel (
    parallelize
) where

import           Common                      (Color, (.:))
import           Control.Parallel.Strategies
import           Tracing.Ray

parallelize :: NFData b => (a -> b) -> [a] -> [b]
{-# SPECIALIZE parallelize :: (Ray -> Color) -> [Ray] -> [Color] #-}
parallelize = withStrategy (parBuffer 100 rdeepseq) .: map
