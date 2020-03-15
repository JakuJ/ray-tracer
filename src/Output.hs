module Output where

import           Common          (Color)
import           Env

import           Codec.BMP
import           Control.Lens    ((^.))
import           Data.ByteString (pack)
import           Data.Word       (Word8)
import           Linear          (V4 (..))

type Image = [Word8]

pixelsToImage :: [Color] -> Image
pixelsToImage = concatMap (\(V4 x y z w) -> map (floor . (* 255)) [x, y, z, w])

saveImage :: Env -> String -> Image -> IO ()
saveImage (Env w h _) filename img = writeBMP filename $ packRGBA32ToBMP w h $ pack img
