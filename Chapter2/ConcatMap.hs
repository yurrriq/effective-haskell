module ConcatMap where

concatMapL :: (a -> [b]) -> [a] -> [b]
concatMapL f = foldl go []
  where
    go acc x = acc <> f x

concatMapR :: (a -> [b]) -> [a] -> [b]
concatMapR f = foldr go []
  where
    go x acc = f x <> acc
