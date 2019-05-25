module Course1.Week3 where

import Data.List

countComparison :: IO ()
countComparison = do
  file <- readFile "QuickSort.txt"
  let ints = Data.List.map read (lines file) :: [Integer]
  putStrLn "First as pivot:"
  print . firstAsPivot $ ints
  putStrLn "Last as pivot:"
  print . lastAsPivot $ ints
  putStrLn "Median as pivot:"
  print . medianAsPivot $ ints

firstAsPivot :: Ord a => [a] -> Int
firstAsPivot [] = 0
firstAsPivot xs =
  let pivot = head xs
      left = adjustLeftSublist [x | x <- xs, x < pivot]
      right = [x | x <- xs, x > pivot]
      ltPivotComparisons = firstAsPivot left
      gtPivotComparisons = firstAsPivot right
      comparisons = length xs - 1
  in
    ltPivotComparisons + gtPivotComparisons + comparisons

lastAsPivot :: Ord a => [a] -> Int
lastAsPivot [] = 0
lastAsPivot xs =
  let xs' = swapFirstAndLast xs
      comparisons = length xs' - 1
      pivot = head xs
      left = adjustLeftSublist [x | x <- xs', x < pivot]
      right = [x | x <- xs', x > pivot]
      ltPivotComparisons = lastAsPivot left
      gtPivotComparisons = lastAsPivot right
  in
    ltPivotComparisons + gtPivotComparisons + comparisons

medianAsPivot :: Ord a => [a] -> Int
medianAsPivot [] = 0
medianAsPivot [_] = 0
medianAsPivot xs =
  let l = length xs
      middleE = if even l then xs !! (quot l 2 - 1) else xs !! quot l 2
      pivot = Data.List.sort [head xs, middleE, last xs] !! 1
      comparisons = l - 1
      listMinusPivot = Data.List.filter (/= pivot) xs
      ltPivotComparisons = medianAsPivot [x | x <- listMinusPivot, x < pivot]
      gtPivotComparisons = medianAsPivot [x | x <- listMinusPivot, x >= pivot]
  in
    ltPivotComparisons + gtPivotComparisons + comparisons

adjustLeftSublist :: [a] -> [a]
adjustLeftSublist [] = []
adjustLeftSublist [x] = [x]
adjustLeftSublist [x,y] = [y,x]
adjustLeftSublist xs = last xs : Data.List.take (length xs - 1) xs

swapFirstAndLast :: [a] -> [a]
swapFirstAndLast [] = []
swapFirstAndLast [x] = [x]
swapFirstAndLast [x,y] = [y,x]
swapFirstAndLast xs = last xs : Data.List.drop 1 (Data.List.take (length xs - 1) xs) ++ [head xs]
