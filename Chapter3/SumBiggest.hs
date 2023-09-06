module SumBiggest where

import Data.Function (on)
import Data.List (intercalate, sort, sortBy)
import Data.Ord (Down (..), comparing)

sumBiggest :: [[Int]] -> String
sumBiggest allNums =
  let getBiggests :: [Int] -> [Int]
      getBiggests = filter =<< (==) . maximum

      getSmallests :: [Int] -> [Int]
      getSmallests = filter =<< (==) . minimum

      differences :: ([Int], [Int]) -> Int
      differences = uncurry ((-) `on` sum)

      allBiggests :: [[Int]]
      allBiggests = map getBiggests allNums

      allSmallests :: [[Int]]
      allSmallests = map getSmallests allNums

      sizePairs :: [([Int], [Int])]
      sizePairs = zip allBiggests allSmallests

      differences' :: [String]
      differences' = map (show . differences) sizePairs
   in intercalate "," differences'
