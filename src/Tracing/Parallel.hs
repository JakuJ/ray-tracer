module Tracing.Parallel where

import           Common
import           Control.Parallel.Strategies
import           Tracing.Ray

-- |Apply a mapping over a list of elements in parallel using a buffering strategy.
parallelize :: NFData b => (a -> b) -> [a] -> [b]
{-# SPECIALIZE parallelize :: (Ray -> Color) -> [Ray] -> [Color] #-}
parallelize = withStrategy (parBuffer 100 rdeepseq) .: map
