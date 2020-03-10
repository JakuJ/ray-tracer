{-# LANGUAGE TemplateHaskell #-}

module Output where

import           Codec.BMP
import           Control.Lens    ((^.))
import           Data.ByteString (pack)
import           Data.Word       (Word8)
import           Env
import           Linear          (V4 (..))

type Image = [Word8]
type Pixel = V4 Int

pixelsToImage :: [Pixel] -> Image
pixelsToImage = concatMap (\(V4 x y z w) -> map fromIntegral [x, y, z, w])

saveImage :: Env -> String -> Image -> IO ()
saveImage env filename img = do
    let rgba = pack img
    let bmp = packRGBA32ToBMP (env ^. imageWidth) (env ^. imageHeight) rgba
    writeBMP filename bmp
