module Output where

import           Codec.BMP
import           Data.ByteString (pack)
import           Data.Word
import           Linear

imageWidth, imageHeight :: Int
imageWidth = 800
imageHeight = 600

type Image = [Word8]
type Pixel = V4 Int

pixelsToImage :: [Pixel] -> Image
pixelsToImage = foldMap (\(V4 x y z w) -> map fromIntegral [x, y, z, w])

saveImage :: String -> Image -> IO ()
saveImage filename words = do
    let rgba = pack words
    let bmp = packRGBA32ToBMP imageWidth imageHeight rgba
    writeBMP filename bmp
