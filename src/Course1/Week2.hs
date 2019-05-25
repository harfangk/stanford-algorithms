module Course1.Week2 where

import Data.List

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
