module Reverse where

reverseL :: [a] -> [a]
reverseL = foldl go []
  where
    go zs x = x : zs

reverseR :: [a] -> [a]
reverseR = foldr go []
  where
    go x zs = zs <> [x]
