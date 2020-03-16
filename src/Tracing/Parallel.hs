module Tracing.Parallel (
    parallelize
) where

import           Env                         (Env, imageHeight, imageWidth)

import           Control.DeepSeq             (NFData, force)
import           Control.Lens                ((^.))
import           Control.Parallel.Strategies (rpar, rseq, runEval)
import           Tracing.Ray                 (Ray (..))

parallelize :: NFData b => Env -> (a -> b) -> [a] -> [b]
parallelize = parallelizeEval

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (c, cs) = splitAt n xs in c : chunksOf n cs

parallelizeEval :: NFData b => Env -> (a -> b) -> [a] -> [b]
parallelizeEval env f lst = runEval $ do
    ts <- mapM (rpar . force . map f) chunks
    mapM_ rseq ts
    return $ concat ts
    where
        chunks = chunksOf chunkSize lst
        chunkSize = (env ^. imageWidth * env ^. imageHeight) `div` 1000
