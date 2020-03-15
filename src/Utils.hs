module Utils where

filterZipMaybe :: (a -> Maybe b) -> [a] -> [(a, b)]
filterZipMaybe _ [] = []
filterZipMaybe f (x:xs) = case f x of
  Just b  -> (x, b) : filterZipMaybe f xs
  Nothing -> filterZipMaybe f xs
