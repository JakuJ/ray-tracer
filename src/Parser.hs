module Parser (
    parseScene
) where

import           Shapes (Scene)

parseScene :: FilePath -> IO Scene
parseScene = fmap (map read . lines) . readFile
