module Course1 where

import qualified Data.List
import qualified Data.Set
import qualified Data.Map as M
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

instance Eq Vertex where
  (==) (Vertex i1 _) (Vertex i2 _) = (==) i1 i2

instance Ord Vertex where
  compare (Vertex i1 _) (Vertex i2 _) = compare i1 i2

getEdges :: Vertex -> Data.Set.Set Int
getEdges (Vertex _ edges) =
  edges

getIndex :: Vertex -> Int
getIndex (Vertex i _) =
  i

chooseRandomVertex :: System.Random.StdGen -> (M.Map Int [Int]) -> ((Int, [Int]), System.Random.StdGen)
chooseRandomVertex rndGen vertices =
  let (i, newGen) = System.Random.randomR (0, (M.size vertices) - 1) rndGen
      key = (M.keys vertices) !! i
      edges = vertices M.! key
      result = (key, edges)
  in
    (result, newGen)


chooseRandomEdgeHead :: System.Random.StdGen -> [Int] -> (Int, System.Random.StdGen)
chooseRandomEdgeHead rndGen edges =
  let (i, newGen) = System.Random.randomR (0, (Data.List.length edges) - 1) rndGen
  in
    (edges !! i, newGen)
{-
contract
1. randomly choose v
2. randomly choose u among vertices adjacent to v
3. make edges between u and its adjacent vertices, called newEdges, point at v
4. remove u
5. remove self-referencing edges from v
6. add edges v-newEdges to v
-}

contractVertices :: (M.Map Int [Int], System.Random.StdGen) -> (M.Map Int [Int], System.Random.StdGen)
contractVertices (vertices, rndGen) =
  let
    ((vi, vEdges), rndGen_) = chooseRandomVertex rndGen vertices
    (ui, rndGen__) = chooseRandomEdgeHead rndGen_ vEdges
    uEdges = vertices M.! ui
    uEndsChangedToV =
      Data.List.foldl (\acc w -> M.insert w (Data.List.map (\x -> if x == ui then vi else x) (acc M.! w)) acc) vertices uEdges
    wEndsAddedToV =
      M.insert vi (Data.List.filter (/= vi) ((uEndsChangedToV M.! vi) ++ uEdges)) uEndsChangedToV
    uRemoved = M.delete ui wEndsAddedToV
  in
    (uRemoved, rndGen__)

kargerMinCut :: (M.Map Int [Int], System.Random.StdGen) -> IO (M.Map Int [Int], System.Random.StdGen)
kargerMinCut (vertices, rndGen) =
  if M.size vertices <= 2 then
    pure (vertices, rndGen)
  else
    do
      kargerMinCut . contractVertices $ (vertices, rndGen)

kargerMinCutMain :: Int -> IO ()
kargerMinCutMain runs = do
  file <- readFile "src/kargerMinCut.txt"
  let strings = Data.List.map words (lines file) :: [[String]]
  let ints = Data.List.map (Data.List.map read) strings :: [[Int]]
  let vertexMap = Data.List.foldl (\vAcc v -> M.insert (Data.List.head v) (Data.List.drop 1 v) vAcc) M.empty ints
  rndGen <- System.Random.getStdGen
  kargerMinCutMain_ runs vertexMap rndGen

kargerMinCutMain_ :: Int -> M.Map Int [Int] -> System.Random.StdGen -> IO ()
kargerMinCutMain_ runs vertexMap rndGen = do
  if runs == 0 then
    pure ()
  else do
    (resultMap, rndGen_) <- kargerMinCut (vertexMap, rndGen)
    putStrLn $ "Run: " ++ show runs ++ ", Min cut: " ++ show (Data.List.length (resultMap M.! ((M.keys resultMap) !! 0)))
    kargerMinCutMain_ (runs - 1) vertexMap rndGen_
