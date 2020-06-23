module Output (
  saveImage
) where

import           Common
import           Env

import           Codec.Picture
import qualified Data.Vector.Storable as SV (Vector, fromListN)
import           Linear

pixelsToImage :: Int -> [Color] -> SV.Vector (PixelBaseComponent PixelRGBA8)
pixelsToImage size = SV.fromListN size . concatMap toPixel
  where
    toPixel (V3 r g b) = [floor (r * 255), floor (g * 255), floor (b * 255), 255]

-- |Saves the list of pixels in row-major order as a bitmap image.
saveImage :: Env -> FilePath -> [Color] -> IO ()
saveImage (Env w h _) path img = let
  image = Image w h $ pixelsToImage (w * h * 4) img :: Image PixelRGBA8
  in writeBitmap path image
