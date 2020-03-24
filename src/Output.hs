module Output where

import           Common               (Color)
import           Env                  (Env (..))

import           Codec.Picture
import qualified Data.Vector.Storable as SV (Vector, fromListN)
import           Linear               (V4 (..))

pixelsToImage :: Int -> [Color] -> SV.Vector (PixelBaseComponent PixelRGBA8)
pixelsToImage size = SV.fromListN size . concatMap toPixel
  where
    toPixel (V4 r g b _) = [floor (r * 255), floor (g * 255), floor (b * 255), 255]

saveImage :: Env -> String -> [Color] -> IO ()
saveImage (Env w h _) path img = let
  image = Image w h $ pixelsToImage (w * h * 4) img :: Image PixelRGBA8
  in writeBitmap path image
