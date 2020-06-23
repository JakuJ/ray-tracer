module Main (main) where

import           Env                (defaultEnv)
import           Output             (saveImage)
import           Parser             (parseScene)
import           Tracing.Raytracer  (render)

import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)
import           System.FilePath

renderScene :: FilePath -> IO ()
renderScene filename = do
    tuple <- parseScene filename
    case tuple of
        Just (s, e)  -> let env = fromMaybe defaultEnv e in
            saveImage env (dropExtension filename <.> "bmp") $ render env s
        Nothing -> putStrLn $ "Parsing failure in " ++ filename

main :: IO ()
main = mapM_ renderScene =<< getArgs
