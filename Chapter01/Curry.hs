module Curry where

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

uncurriedAddition :: (Num a) => (a, a) -> a
uncurriedAddition = uncurry' (+)

addition :: (Num a) => a -> a -> a
addition = curry' uncurriedAddition

addOne :: (Num a) => a -> a
addOne = addition 1

addTwo :: (Num a) => a -> a
addTwo = addition 2
