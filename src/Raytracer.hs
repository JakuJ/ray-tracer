{-# LANGUAGE TemplateHaskell #-}

module Raytracer where

import           Control.Lens                ((^.))
import           Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import           Data.Function               (on)
import           Data.List                   (minimum)
import           Data.Maybe                  (listToMaybe, mapMaybe)
import           Env
import           Linear                      hiding (trace)
import           Output
import           Shapes

castDiv :: Integral a => a -> a -> Float
castDiv = (/) `on` fromIntegral

-- TODO: Actually use the camera
makeRays :: Env -> [Ray]
makeRays (Env width height camera) = do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let nx = 2 * (x `castDiv` width) - 1
    let ny = 2 * (y `castDiv` height) - 1
    return $ Ray (camera ^. position) (normalize (V3 (aspect * nx) ny (-1)))
    where
        aspect = width `castDiv` height

parallelize :: Env -> (a -> b) -> [a] -> [b]
parallelize env f = withStrategy (parListChunk chunkSize rseq) . map f
    where
        chunkSize = env ^. imageWidth * env ^. imageHeight `div` 4

render :: Env -> Scene -> Image
render env scene = pixelsToImage $ parallelize env (trace scene) rays
    where
        rays = makeRays env

minMaybe :: Ord a => [a] -> Maybe a
minMaybe lst = if null lst then Nothing else Just $ minimum lst

trace :: Scene -> Ray -> Pixel
trace scene ray = color $ minMaybe $ collisions ray scene
    where
        color Nothing  = V4 0 0 0 255
        color (Just d) = let k = floor (255 * exp (- 0.2 * d)) in V4 k k k 255

collisions :: Ray -> Scene -> [Float]
collisions ray = mapMaybe (collide ray)
