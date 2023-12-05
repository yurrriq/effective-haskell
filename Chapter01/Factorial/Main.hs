module Main where

main :: IO ()
main = print (factorial 42)

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
