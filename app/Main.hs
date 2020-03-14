module Main (main) where

import           Linear        (V3 (..), V4 (..), zero)
import           System.Random (mkStdGen, randoms)

import           Env           (defaultEnv)
import           Materials     (Material (..))
import           Output        (saveImage)
import           Parser        (parseScene)
import           Raytracer     (render)
import           Shapes        (Shape (..))

randomPoints3D :: [(Float, Float, Float)]
randomPoints3D = zip3 (randoms (mkStdGen 123)) (randoms (mkStdGen 456)) (randoms (mkStdGen 789))

glossy_white = Material (V4 1 1 1 1) 0 1.0
transparent = Material (V4 1 1 1 1) 1

testScene :: [Shape]
testScene = Plane (V3 0 0 1) glossy_white : do
    x <- [-1, 0, 1]
    y <- [3, 4, 5]
    let refr = case y of
            3 -> 1.3
            4 -> 1.5
            5 -> 1.8
    return $ Sphere (V3 x y 5) 0.4 $ transparent refr

main :: IO ()
main = saveImage defaultEnv "obraz.bmp" $ render defaultEnv testScene
