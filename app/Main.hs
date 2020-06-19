module Main (main) where

import           Env               (defaultEnv)
import           Data.Maybe        (fromMaybe)
import           System.Directory  (listDirectory)
import           Output            (saveImage)
import           Parser            (parseScene)
import           Tracing.Raytracer (render)

listScenes :: IO [FilePath]
listScenes = listDirectory "scenes"

renderScene :: FilePath -> IO ()
renderScene filename = do
    tuple <- parseScene $ "scenes/" ++ filename
    let name = takeWhile (/= '.') filename
    case tuple of
        Just (s, e)  -> let env = (fromMaybe defaultEnv e) in 
            saveImage env ("rendered/" ++ name ++ ".bmp") $ render env s
        Nothing -> putStrLn $ "Parsing failure in " ++ filename

main :: IO ()
main = mapM_ renderScene =<< listScenes