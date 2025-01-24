module Fibs where

fibs :: [Integer]
fibs = 0 : 1 : go fibs (tail fibs)
  where
    go (a : as) (b : bs) =
      a + b : go as bs
