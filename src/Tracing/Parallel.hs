module Tracing.Parallel (
    parallelize
) where

import           Common                      (Color, (.:))
import           Control.Parallel.Strategies

parallelize :: NFData b => (a -> b) -> [a] -> [b]
{-# SPECIALIZE parallelize :: (a -> Color) -> [a] -> [Color] #-}
parallelize = withStrategy (parBuffer 100 rdeepseq) .: map
