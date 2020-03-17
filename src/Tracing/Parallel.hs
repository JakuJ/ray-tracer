module Tracing.Parallel (
    parallelize
) where

import           Control.DeepSeq             (NFData)
import           Control.Parallel.Strategies

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

parallelize :: NFData b => (a -> b) -> [a] -> [b]
parallelize f = withStrategy (parBuffer 100 rdeepseq) . map f

-- 1000 chunksSize
-- 22.15 GB on heap
-- 1.61 GB in GC
-- 19 MB max residency
-- 18 MB total