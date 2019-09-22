module Course2.Week3 where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified MinHeap as MinHeap
import qualified MaxHeap as MaxHeap

type WeightedGraph = Map.Map Int [(Int, Int)]


main :: IO ()
main = do
  file <- readFile "src/Median.txt"
  let ns =  List.map read . lines $ file :: [Int]
      (_, _, medians) = List.foldl median (MaxHeap.empty, MinHeap.empty, [] :: [Int]) ns
      result = mod (List.foldl (+) 0 ( medians)) 10000
  print result


median :: (MaxHeap.MaxHeap Int Int, MinHeap.MinHeap Int Int, [Int]) -> Int -> (MaxHeap.MaxHeap Int Int, MinHeap.MinHeap Int Int, [Int])
median (h1, h2, medians) i =
  case (h1Size, h2Size) of
    (0, 0) ->
      (MaxHeap.insert (i, i) h1, h2, [i])
    (1, 0) ->
      let
        (m, _, _) = MaxHeap.extract h1
        smallerVal = min m i
        largerVal = max m i
        h1' = MaxHeap.insert (smallerVal, smallerVal) MaxHeap.empty
        h2' = MinHeap.insert (largerVal, largerVal) MinHeap.empty
      in
        (h1', h2', smallerVal:medians)
    (0, 1) ->
      error "median: Heap of larger elements cannot have more elements than heap of smaller elements"
    (h1Size', h2Size') ->
      let
        h1Max = fst . MaxHeap.find $ h1
        h2Min = fst . MinHeap.find $ h2
      in
      case compare h1Size' h2Size' of
        EQ ->
          if i > h2Min then
            let
              (_, _, h2') = MinHeap.extract h2
              h1' = MaxHeap.insert (h2Min, h2Min) h1
              h2'' = MinHeap.insert (i,i) h2'
            in
              (h1', h2'', (fst . MaxHeap.find $ h1'):medians)
          else
            let
              h1' = MaxHeap.insert (i,i) h1
            in
              (h1', h2, (fst . MaxHeap.find $ h1'):medians)
        GT ->
          if i >= h2Min then
            let
              h2' = MinHeap.insert (i,i) h2
            in
              (h1, h2', h1Max:medians)
          else
            let
              (_, _, h1') = MaxHeap.extract h1
              largerVal = max h1Max i
              smallerVal = min h1Max i
              h1'' = MaxHeap.insert (smallerVal, smallerVal) h1'
              h2' = MinHeap.insert (largerVal, largerVal) h2
            in
              (h1'', h2', (fst . MaxHeap.find $ h1''):medians)
        LT ->
          error "median: Heap of larger elements cannot have more elements than heap of smaller elements"
  where
    h1Size = MaxHeap.size h1
    h2Size = MinHeap.size h2
