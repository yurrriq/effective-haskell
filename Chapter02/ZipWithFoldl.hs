module ZipWithFoldl where

import Prelude hiding (zipWith)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f xs ys = reverse (fst (foldl go ([], xs) ys))
  where
    go (acc, []) _ = (acc, [])
    go (acc, x' : xs') y = (f x' y : acc, xs')
