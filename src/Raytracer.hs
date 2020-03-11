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
import           Types

castDiv :: Integral a => a -> a -> Float
castDiv = (/) `on` fromIntegral

-- TODO: Actually use the camera
makeRays :: Env -> [Ray]
makeRays (Env width height camera) = do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let nx = 2 * (x `castDiv` width) - 1
    let ny = 2 * (y `castDiv` height) - 1
    return $ Ray (camera ^. position) ((normalize . vector) (V3 (aspect * nx) ny (-1)))
    where
        aspect = width `castDiv` height

parallelize :: Env -> (a -> b) -> [a] -> [b]
parallelize env f = withStrategy (parListChunk chunkSize rseq) . map f
    where
        chunkSize = env ^. imageWidth * env ^. imageHeight `div` 40

render :: Env -> Scene -> Image
render env scene = pixelsToImage $ parallelize env (trace scene) rays
    where
        rays = makeRays env

minMaybe :: Ord a => [a] -> Maybe a
minMaybe lst = if null lst then Nothing else Just $ minimum lst

combine :: [Color] -> Color
combine lst = if null lst then zero else maximum lst

trace :: Scene -> Ray -> Pixel
trace scene ray = combine $ collisions ray scene

collisions :: Ray -> Scene -> [Color]
collisions ray = mapMaybe (intColor ray)
