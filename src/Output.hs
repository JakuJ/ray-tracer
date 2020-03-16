module Output where

import           Common               (Color)
import           Env

import           Codec.Picture
import           Control.Lens         ((^.))
import           Data.Vector.Storable (fromList)
import           Data.Word            (Word8)
import           Linear

pixelsToImage :: [Color] -> [PixelBaseComponent PixelRGBA8]
pixelsToImage = concatMap toPixel
    where
        toPixel (V4 r g b a) = map (floor . (255 *)) [r, g, b, a]

saveImage :: Env -> String -> [Color] -> IO ()
saveImage (Env w h _) filename img = writeBitmap filename image
    where
        image :: Image PixelRGBA8
        image = Image w h $ fromList pixels
        pixels = pixelsToImage img
