module Main (main) where

import           Env               (defaultEnv)
import           Output            (saveImage)
import           Parser            (parseScene)
import           Tracing.Raytracer (render)

main :: IO ()
main = do
    scene <- parseScene "scene.txt"
    case scene of
        Just s  -> saveImage defaultEnv "obraz.bmp" $ render defaultEnv s
        Nothing -> putStrLn "Parsing failure"
