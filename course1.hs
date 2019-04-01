module Course1 where

import qualified Data.List
import qualified Data.Set
import qualified System.Random

karatsuba :: Integer -> Integer -> Integer
karatsuba i j =
  let iDigits = length . show $ i
      jDigits = length . show $ j
  in
      if iDigits < 4 || jDigits < 4 then
        i * j
      else
        let
          m = minimum [quot iDigits 2, quot jDigits 2]
          (a, b) = quotRem i (10 ^ m)
          (c, d) = quotRem j (10 ^ m)
          z2 = karatsuba a c
          z0 = karatsuba b d
          z1 = karatsuba (a + b) (c + d) - z0 - z2
        in
          z2 * (10 ^ (2 * m)) + z1 * (10 ^ m) + z0

countInversion :: IO ()
countInversion = do
  file <- readFile "IntegerArray.txt"
  let ints = Data.List.map read (lines file) :: [Integer]
      (sortedArray, inversions) = count ints
  print inversions


count :: Ord a => [a] -> ([a], Integer)
count xs = count_ (xs, 0)

count_ :: Ord a => ([a], Integer) -> ([a], Integer)
count_ ([], i) = ([], i)
count_ ([x], i) = ([x], i)
count_ (xs, i) =
  let
    (left, right) = halve xs
    (sortedLeft, z0) = count_ (left, 0)
    (sortedRight, z1) = count_ (right, 0)
    z2 = countCrossInversion sortedLeft sortedRight 0
  in
    (merge sortedLeft sortedRight, z0 + z1 + z2)

halve :: [a] -> ([a],[a])
halve xs =
  let lhx = length xs `div` 2
  in (Data.List.take lhx xs, Data.List.drop lhx xs)


countCrossInversion :: Ord a => [a] -> [a] -> Integer -> Integer
countCrossInversion _ [] acc = acc
countCrossInversion [] _ acc = acc
countCrossInversion (x:xs) (y:ys) acc | x > y = countCrossInversion (x:xs) ys (acc + (toInteger . length $ (x:xs)))
                                 | otherwise = countCrossInversion xs (y:ys) acc

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

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
medianAsPivot [x] = 0
medianAsPivot xs =
  let l = length xs
      middleE = if even l then xs !! (quot l 2 - 1) else xs !! quot l 2
      pivot = sort [head xs, middleE, last xs] !! 1
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

{-
1. create a representation of graph
2. randomly choose a vertex v
3. randomly choose an edge {v,u}
4. add all edges {u,x} to v
5. remove u
6. make all edges {x,u} to point at v
7. remove self-referencing edges {v,v}
repeat until two vertices are left, then count edges
-}

newtype Graph = Graph (Data.Set.Set Vertex)
data Vertex = Vertex Int (Data.Set.Set Int)

