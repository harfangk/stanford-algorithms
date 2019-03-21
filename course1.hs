module Course1 (karatsuba) where

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
  let ints = map read (lines file) :: [Integer]
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
  in (take lhx xs, drop lhx xs)


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
