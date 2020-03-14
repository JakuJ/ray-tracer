module Parser (
    parseScene
) where

import           Shapes (Shape)

parseScene :: FilePath -> IO [Shape]
parseScene = fmap (map read . lines) . readFile
