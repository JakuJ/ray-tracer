module Output where

import           Codec.BMP
import           Control.Lens    ((^.))
import           Data.ByteString (pack)
import           Data.Word       (Word8)
import           Env
import           Linear          (V4 (..))

import           Materials       (Color)

type Image = [Word8]

pixelsToImage :: [Color] -> Image
pixelsToImage = concatMap (\(V4 x y z w) -> map (floor . (* 255)) [x, y, z, w])

saveImage :: Env -> String -> Image -> IO ()
saveImage (Env w h _) filename img = writeBMP filename $ packRGBA32ToBMP w h $ pack img
