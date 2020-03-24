module Tracing.Parallel (
    parallelize
) where

import           Common                      (Color)
import           Control.Parallel.Strategies

parallelize :: NFData b => (a -> b) -> [a] -> [b]
parallelize f = withStrategy (parBuffer 100 rdeepseq) . map f
{-# SPECIALIZE parallelize :: (a -> Color) -> [a] -> [Color] #-}
