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
      result = dijkstra graph (MinNode 0 1 []) minDists
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

dijkstra :: WeightedGraph -> MinHeap Int Int -> Map.Map Int Int -> Map.Map Int Int
dijkstra g h minDists | Map.null g || isEmptyMin h = minDists
dijkstra g h minDists =
  case Map.lookup nodeLabel g of
    Nothing -> dijkstra g' h' minDists
    Just es -> dijkstra g' (updateHeap d es h') minDists'
  where
    (d, nodeLabel, h') = extractMin h
    g' = Map.delete nodeLabel g
    minDists' = Map.insertWith (\newValue oldValue -> minimum [newValue, oldValue]) nodeLabel d minDists


updateHeap :: Int -> [(Int, Int)] -> MinHeap Int Int -> MinHeap Int Int
updateHeap d es h =
  mergeMin h (mergeMinAll (List.map (\(edgeHead, edgeWeight) -> MinNode (edgeWeight + d) edgeHead []) es) )

data MinHeap k v = MinEmpty | MinNode k v [MinHeap k v]

instance (Show k, Show v) => Show (MinHeap k v) where
  show h =
    case h of
      MinEmpty -> "MinHeap.Empty"
      MinNode k v hs -> "MinHeap.Node(" ++ show k ++ ", " ++ show v ++ ", " ++ (show . length $ hs) ++ ")"

insertMin :: (Ord k) => (k, v) -> MinHeap k v -> MinHeap k v
insertMin (key, val) = mergeMin (MinNode key val [])

mergeMin :: (Ord k) => MinHeap k v -> MinHeap k v -> MinHeap k v
mergeMin h MinEmpty = h
mergeMin MinEmpty h = h
mergeMin h1@(MinNode key1 value1 hs1) h2@(MinNode key2 value2 hs2)
  | key1 < key2 = MinNode key1 value1 (h2:hs1)
  | otherwise = MinNode key2 value2 (h1:hs2)

mergeMinAll :: (Ord k) => [MinHeap k v] -> MinHeap k v
mergeMinAll [] = MinEmpty
mergeMinAll [h] = h
mergeMinAll (h:h':hs) = mergeMin (mergeMin h h') (mergeMinAll hs)

isEmptyMin :: MinHeap k v -> Bool
isEmptyMin MinEmpty = True
isEmptyMin _ = False

extractMin :: (Ord k) => MinHeap k v -> (k, v, MinHeap k v)
extractMin MinEmpty = error "MinHeap extractMin: empty MinHeap"
extractMin (MinNode key value hs) = (key, value, mergeMinAll hs)

sizeMin :: (Ord k) => MinHeap k v -> Int
sizeMin MinEmpty = 0
sizeMin (MinNode _ _ hs) = (1 + (foldl (+) 0 (List.map sizeMin hs)))

findMin :: (Ord k) => MinHeap k v -> (k, v)
findMin MinEmpty = error "MinHeap.findMin: empty heap"
findMin (MinNode k v _) = (k, v)

data MaxHeap k v = MaxEmpty | MaxNode k v [MaxHeap k v]

instance (Show k, Show v) => Show (MaxHeap k v) where
  show h =
    case h of
      MaxEmpty -> "MaxHeap.Empty"
      MaxNode k v hs -> "MaxHeap.Node(" ++ show k ++ ", " ++ show v ++ ", " ++ (show . length $ hs) ++ ")"

insertMax :: (Ord k) => (k, v) -> MaxHeap k v -> MaxHeap k v
insertMax (key, val) = mergeMax (MaxNode key val [])

mergeMax :: (Ord k) => MaxHeap k v -> MaxHeap k v -> MaxHeap k v
mergeMax h MaxEmpty = h
mergeMax MaxEmpty h = h
mergeMax h1@(MaxNode key1 value1 hs1) h2@(MaxNode key2 value2 hs2)
  | key1 > key2 = MaxNode key1 value1 (h2:hs1)
  | otherwise = MaxNode key2 value2 (h1:hs2)

mergeMaxAll :: (Ord k) => [MaxHeap k v] -> MaxHeap k v
mergeMaxAll [] = MaxEmpty
mergeMaxAll [h] = h
mergeMaxAll (h:h':hs) = mergeMax (mergeMax h h') (mergeMaxAll hs)

isEmptyMax :: MaxHeap k v -> Bool
isEmptyMax MaxEmpty = True
isEmptyMax _ = False

extractMax :: (Ord k) => MaxHeap k v -> (k, v, MaxHeap k v)
extractMax MaxEmpty = error "MaxHeap extractMax: empty MaxHeap"
extractMax (MaxNode key value hs) = (key, value, mergeMaxAll hs)

findMax :: (Ord k) => MaxHeap k v -> (k, v)
findMax MaxEmpty = error "MaxHeap.findMax: empty heap"
findMax (MaxNode k v _) = (k, v)

sizeMax :: (Ord k) => MaxHeap k v -> Int
sizeMax MaxEmpty = 0
sizeMax (MaxNode _ _ hs) = (1 + (foldl (+) 0 (List.map sizeMax hs)))

medianMain :: IO ()
medianMain = do
  file <- readFile "src/Median.txt"
  let ns =  List.map read . lines $ file :: [Int]
      (_, _, medians) = List.foldl median (MaxEmpty, MinEmpty, [] :: [Int]) ns
      result = mod (List.foldl (+) 0 ( medians)) 10000
  print result


median :: (MaxHeap Int Int, MinHeap Int Int, [Int]) -> Int -> (MaxHeap Int Int, MinHeap Int Int, [Int])
median (h1, h2, medians) i =
  case (h1Size, h2Size) of
    (0, 0) ->
      (insertMax (i, i) h1, h2, [i])
    (1, 0) ->
      let
        (m, _, _) = extractMax h1
        smallerVal = min m i
        largerVal = max m i
        h1' = insertMax (smallerVal, smallerVal) MaxEmpty
        h2' = insertMin (largerVal, largerVal) MinEmpty
      in
        (h1', h2', smallerVal:medians)
    (0, 1) ->
      error "median: Heap of larger elements cannot have more elements than heap of smaller elements"
    _ ->
      let
        h1Max = fst . findMax $ h1
        h2Min = fst . findMin $ h2
      in
      case compare h1Size h2Size of
        EQ ->
          if i > h2Min then
            let
              (_, _, h2') = extractMin h2
              h1' = insertMax (h2Min, h2Min) h1
              h2'' = insertMin (i,i) h2'
            in
              (h1', h2'', (fst . findMax $ h1'):medians)
          else
            let
              h1' = insertMax (i,i) h1
            in
              (h1', h2, (fst . findMax $ h1'):medians)
        GT ->
          if i >= h2Min then
            let
              h2' = insertMin (i,i) h2
            in
              (h1, h2', h1Max:medians)
          else
            let
              (_, _, h1') = extractMax h1
              largerVal = max h1Max i
              smallerVal = min h1Max i
              h1'' = insertMax (smallerVal, smallerVal) h1'
              h2' = insertMin (largerVal, largerVal) h2
            in
              (h1'', h2', (fst . findMax $ h1''):medians)
        LT ->
          error "median: Heap of larger elements cannot have more elements than heap of smaller elements"
  where
    h1Size = sizeMax h1
    h2Size = sizeMin h2
