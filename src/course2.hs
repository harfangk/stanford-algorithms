module Course2 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Debug.Trace

type Graph = Map.Map Int [Int]

sccMain :: IO ()
sccMain = do
  file <- readFile "src/SCC.txt"
  let parsedList =  List.map (List.map read . words) . lines $ file :: [[Int]]
      baseG = Map.fromAscList (List.map (\k -> (k,[])) [1.. 875714]) :: Graph
      g = List.foldr (\x acc -> Map.insertWith (++) (List.head x) [List.last x] acc) baseG parsedList :: Graph
      revG = List.foldr (\x acc -> Map.insertWith (++) (List.last x) [List.head x] acc) baseG parsedList :: Graph
      firstRunResult =
        Map.foldrWithKey (\k _ acc ->
                            if Set.member k (fst acc) then
                              acc
                            else
                              dfsForFinishingTime revG k acc) (Set.empty, []) revG
      (_, resultSccs, _) = List.foldl' (\(visited, sccs, leader) x ->
                                          if Set.member x visited then
                                            (visited, sccs, leader)
                                          else
                                            dfsForScc g x (visited, sccs, x)) (Set.empty, Map.empty, 0) (snd firstRunResult)
  print . List.take 5 . List.sortBy (flip compare) . List.map (List.length . snd) . Map.toList $ resultSccs

dfsForFinishingTime :: Graph -> Int -> (Set.Set Int, [Int]) -> (Set.Set Int, [Int])
dfsForFinishingTime graph source (visited, vByF) =
  if Set.member source visited then
    (visited, vByF)
  else
    let
      adjacentVs = graph Map.! source
      newVisited = Set.insert source visited
      result =
        if List.null adjacentVs || Set.isSubsetOf (Set.fromList adjacentVs) newVisited then
          (newVisited, vByF)
        else
          (List.foldr (\i acc -> dfsForFinishingTime graph i acc) (newVisited, vByF) adjacentVs)
    in
      (fst result, source : snd result)

dfsForScc :: Graph -> Int -> (Set.Set Int, Map.Map Int [Int], Int) -> (Set.Set Int, Map.Map Int [Int], Int)
dfsForScc graph source (visited, sccs, leader) =
  if Set.member source visited then
    (visited, sccs, leader)
  else
    let
      adjacentVs = graph Map.! source
      newVisited = Set.insert source visited
      (resultVisited, resultSccs, resultLeader) =
        if List.null adjacentVs || Set.isSubsetOf (Set.fromList adjacentVs) visited then
           (newVisited, sccs, leader)
        else
           (List.foldr (\x acc -> dfsForScc graph x acc) (newVisited, sccs, leader) adjacentVs)
    in
      (resultVisited, Map.insertWith (++) leader [source] resultSccs, resultLeader)

type WeightedGraph = Map.Map Int [(Int, Int)]

dijkstraMain :: IO ()
dijkstraMain = do
  file <- readFile "src/dijkstraData.txt"
  let graph =  List.foldl (\acc (v, es) -> Map.insert v es acc) Map.empty . List.map (parseAdjacencyList . words) . lines $ file :: WeightedGraph
      minDists = Map.fromAscList (List.map (\k -> (k,1000000)) [1.. 200])
      result = dijkstra graph (Node 0 1 []) minDists
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

dijkstra :: WeightedGraph -> Heap Int Int -> Map.Map Int Int -> Map.Map Int Int
dijkstra g h minDists | Map.null g || isEmpty h = minDists
dijkstra g h minDists =
  case Map.lookup nodeLabel g of
    Nothing -> dijkstra g' h' minDists
    Just es -> dijkstra g' (updateHeap d es h') minDists'
  where
    (d, nodeLabel, h') = extractMin h
    g' = Map.delete nodeLabel g
    minDists' = Map.insertWith (\newValue oldValue -> minimum [newValue, oldValue]) nodeLabel d minDists


updateHeap :: Int -> [(Int, Int)] -> Heap Int Int -> Heap Int Int
updateHeap d es h =
  merge h (mergeAll (List.map (\(edgeHead, edgeWeight) -> Node (edgeWeight + d) edgeHead []) es) )

data Heap k v = Empty | Node k v [Heap k v]

instance (Show k, Show v) => Show (Heap k v) where
  show h =
    case h of
      Empty -> "Heap.Empty"
      Node k v hs -> "Heap.Node(" ++ show k ++ ", " ++ show v ++ ", " ++ (show . length $ hs) ++ ")"

insert :: (Ord k, Ord v) => (k, v) -> Heap k v -> Heap k v
insert (key, val) = merge (Node key val [])

merge :: (Ord k, Ord v) => Heap k v -> Heap k v -> Heap k v
merge h Empty = h
merge Empty h = h
merge h1@(Node key1 value1 hs1) h2@(Node key2 value2 hs2)
  | key1 < key2 = Node key1 value1 (h2:hs1)
  | otherwise = Node key2 value2 (h1:hs2)

mergeAll :: (Ord k, Ord v) => [Heap k v] -> Heap k v
mergeAll [] = Empty
mergeAll [h] = h
mergeAll (h:h':hs) = merge (merge h h') (mergeAll hs)

isEmpty :: Heap k v -> Bool
isEmpty Empty = True
isEmpty _ = False

extractMin :: (Ord k, Ord v) => Heap k v -> (k, v, Heap k v)
extractMin Empty = error "Heap extractMin: empty Heap"
extractMin (Node key value hs) = (key, value, mergeAll hs)
