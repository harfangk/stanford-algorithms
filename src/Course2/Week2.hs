module Course2.Week2 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified MinHeap

type WeightedGraph = Map.Map Int [(Int, Int)]

dijkstraMain :: IO ()
dijkstraMain = do
  file <- readFile "src/dijkstraData.txt"
  let graph =  List.foldl (\acc (v, es) -> Map.insert v es acc) Map.empty . List.map (parseAdjacencyList . words) . lines $ file :: WeightedGraph
      minDists = Map.fromAscList (List.map (\k -> (k,1000000)) [1.. 200])
      result = dijkstra graph (MinHeap.Node 0 1 []) minDists
      targets = [7,37,59,82,99,115,133,165,188,197]
      answer = List.map (\i -> result Map.! i) targets
  print answer

parseAdjacencyList :: [String] -> (Int, [(Int, Int)])
parseAdjacencyList [] = (0, [])
parseAdjacencyList (x:xs) =
  (read x, List.map (listToTuple . List.map read . splitByComma) xs)
  where
    listToTuple :: [Int] -> (Int, Int)
    listToTuple is =
      case is of
        [i,j] -> (i,j)
        _ -> (0,0)
    splitByComma :: String -> [String]
    splitByComma s  = case List.dropWhile (== ',') s of
                        "" -> []
                        s' -> w : splitByComma s''
                                      where (w, s'') =
                                             break (== ',') s'

dijkstra :: WeightedGraph -> MinHeap.MinHeap Int Int -> Map.Map Int Int -> Map.Map Int Int
dijkstra g h minDists | Map.null g || isEmptyMin h = minDists
dijkstra g h minDists =
  case Map.lookup nodeLabel g of
    Nothing -> dijkstra g' h' minDists
    Just es -> dijkstra g' (updateHeap d es h') minDists'
  where
    (d, nodeLabel, h') = MinHeap.extract h
    g' = Map.delete nodeLabel g
    minDists' = Map.insertWith (\newValue oldValue -> minimum [newValue, oldValue]) nodeLabel d minDists
