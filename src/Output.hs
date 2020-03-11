{-# LANGUAGE TemplateHaskell #-}

module Output where

import           Codec.BMP
import           Control.Lens    ((^.))
import           Data.ByteString (pack)
import           Data.Word       (Word8)
import           Env
import           Linear          (V4 (..))
import           Types

type Image = [Word8]

pixelsToImage :: [Color] -> Image
pixelsToImage = concatMap (\(V4 x y z w) -> map (floor . (* 255)) [x, y, z, w])

saveImage :: Env -> String -> Image -> IO ()
saveImage env filename img = do
    let rgba = pack img
    let bmp = packRGBA32ToBMP (env ^. imageWidth) (env ^. imageHeight) rgba
    writeBMP filename bmp
