module Main where

import System.Environment (getArgs)

main :: IO ()
main = putStrLn (fizzBuzz 42)

fizzBuzz :: Integer -> String
fizzBuzz = unwords . map go . enumFromTo 1
  where
    go n
      | n `rem` 15 == 0 = "fizzbuzz"
      | n `rem` 5 == 0 = "buzz"
      | n `rem` 3 == 0 = "fizz"
      | otherwise = show n
