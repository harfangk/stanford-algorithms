{-# LANGUAGE TupleSections #-}

module Course4.Week1 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Array as Array
import qualified Data.Array.ST as MArray
import qualified Data.Maybe as Maybe
import qualified Control.Monad.ST as ST
import qualified Control.Monad as CM
import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import qualified Data.PQueue.Prio.Min as MinHeap
import Debug.Trace

type HashTable s k v = Cuckoo.HashTable s k v

main :: IO ()
main = do
  g1File <- readFile "./src/Course4/g3.txt"
  {-
  g2File <- readFile "src/Course4/g2.txt"
  g3File <- readFile "src/Course4/g3.txt"
  -}
  let g1Data@(g1Vc, _, _) = parseData g1File
      --g1Result = floydWarshall2 (buildAdjacencyList g1Data) g1Vc
      g1Result = bellmanFord (buildAdjacencyListByHead g1Data) g1Vc 1
  {-
  let g1Data@(g1Vc, _, _) = addExtraVertexForJohnson . parseData $ g1File
      g1Result = fmap findShortestShortestPath $ johnson (buildAdjacencyList g1Data) g1Vc
      g2Data@(g2Vc, g2Ec, g2Es) = addExtraVertexForJohnson . parseData $ g2File
      g2Result = fmap findShortestShortestPath $ johnson (buildAdjacencyList g2Data) g2Vc
      g3Data@(g3Vc, g3Ec, g3Es) = addExtraVertexForJohnson . parseData $ g3File
      g3Result = fmap findShortestShortestPath $ johnson (buildAdjacencyList g3Data) g3Vc
  print g2Result
  print g3Result
  -}
  let b = seq g1Result ()
  print b

parseData :: String -> (Int, Int, [(Int, Int, Double)])
parseData s =
  (vertexCount, edgeCount, edges)
  where
    fileLines = lines s
    (vertexCount, edgeCount) = parseMetaData . head $ fileLines
    edges = List.map parseEdge . tail $ fileLines
    parseMetaData :: String -> (Int, Int)
    parseMetaData line =
      case List.map read . words $ line of
        [x,y] -> (x, y)
        _ -> error "Invalid data format"
    parseEdge :: String -> (Int, Int, Double)
    parseEdge line =
      case words line of
        (edgeTail:edgeHead:weight:_) -> (read edgeTail, read edgeHead, read weight)
        _ -> error "Invalid data format"

addExtraVertexForJohnson :: (Int, Int, [(Int, Int, Int)]) -> (Int, Int, [(Int, Int, Int)])
addExtraVertexForJohnson (vertexCount, edgeCount, edges) =
  (vertexCount', edgeCount', edges')
  where
    vertexCount' = vertexCount + 1
    edgeCount' = edgeCount + vertexCount
    newEdges = List.map (0,, 0) [1..vertexCount]
    edges' = edges ++ newEdges

buildAdjacencyList :: (Fractional a, Ord a) => (Int, Int, [(Int, Int, a)]) -> IntMap.IntMap [(Int, a)]
buildAdjacencyList (vertexCount, _, edges) =
  List.foldl' (\acc (t, h, w) -> IntMap.update (\edges' -> Just ((h,w):edges')) t acc) initialMap $ edges
  where
    initialMap = IntMap.fromList (List.zip [1..vertexCount] (List.repeat []))

buildAdjacencyListByHead :: (Fractional a, Ord a) => (Int, Int, [(Int, Int, a)]) -> IntMap.IntMap [(Int, a)]
buildAdjacencyListByHead (vertexCount, _, edges) =
  List.foldl' (\acc (t, h, w) -> IntMap.update (\edges' -> Just ((t,w):edges')) h acc) initialMap $ edges
  where
    initialMap = IntMap.fromList (List.zip [1..vertexCount] (List.repeat []))

dijkstra :: IntMap.IntMap [(Int, Double)] -> Int -> Int -> Either String (IntMap.IntMap Double)
dijkstra g vertexCount s =
  if List.any ((>) 0 . snd) . List.concat . IntMap.elems $ g then
    Left "Dijkstra's algorithm halted: negative edge found"
  else
    Right $ dijkstra' g initialHeap initialDists
  where
    initialDists = IntMap.fromList [generator v | v <- [1..vertexCount]]
    initialHeap = MinHeap.insert 0 s MinHeap.empty
    generator v
      | v == s = (v,0)
      | otherwise = (v,1/0)

dijkstra' :: IntMap.IntMap [(Int, Double)] -> MinHeap.MinPQueue Double Int -> IntMap.IntMap Double -> IntMap.IntMap Double
dijkstra' _ h minDists | MinHeap.null h = minDists
dijkstra' g h minDists =
  dijkstra' g h'' minDists'
  where
    ((distU, u), h') = MinHeap.deleteFindMin h
    adjacentVertices = List.map fst $ (IntMap.!) g u
    (h'', minDists') = List.foldl step (h', minDists) adjacentVertices
    step (heap, dists) v =
      if distV > distV' then
        (MinHeap.insert distV' v heap, IntMap.insert v distV' dists)
      else
        (heap, dists)
      where
        distV = (IntMap.!) dists v
        weightUv = snd . head . List.filter (\(edgeHead,_) -> edgeHead == v) $ (IntMap.!) g u
        distV' = distU + weightUv

bellmanFord :: IntMap.IntMap [(Int, Double)] -> Int -> Int -> Either String (Array.Array Int Double)
bellmanFord g vertexCount s =
  if hasNegativeCycle then
    Left "Bellman-Ford algorithm halted: negative cycle found"
  else
    Right resultArray
  where
    memo = Array.array ((0,1), (vertexCount - 1, vertexCount)) [generator (x,y) | x <- [0..(vertexCount - 1)], y <- [1..vertexCount]]
    generator pair@(i,v) =
      if i == 0 then
        if v == s then
          (pair, 0)
        else
          (pair, 1/0)
      else
        (pair, findMin pair)
    -- findMin (i,v) = min ((Array.!) memo (i-1,v)) (List.foldl' (\acc (t,w) -> min acc ((Array.!) memo (i-1,t) + w)) (1/0) ((IntMap.!) g v))
    findMin (i, v) = minimum ( memo Array.! (i - 1, v) : map (\(t, w) -> memo Array.! (i - 1, t) + w) (g IntMap.! v))
    resultList = map (\((_,v), d) -> (v,d)) . filter (\((i,_), _) -> i == vertexCount - 1 ) . Array.assocs $ memo
    resultArray = Array.array (1, vertexCount) resultList
    hasNegativeCycle = any (\(v, d) -> any (\(t, w) -> d > resultArray Array.! t + w) (g IntMap.! v)) resultList
--    hasNegativeCycle = or . concatMap (\(v,d) -> map (\(t,w) -> d > (Array.!) resultArray t + w) ((IntMap.!) g v)) $ resultList

  {-
bellmanFord' :: IntMap.IntMap [(Int, Double)] -> Int -> Int -> Array.Array (Int,Int) Double
bellmanFord' g vertexCount s = MArray.runSTArray $ do
  memo <- MArray.newArray ((0,1), (vertexCount - 1, vertexCount)) (1/0)
  MArray.writeArray memo (0,s) 0
  CM.mapM_ (\pair -> MArray.writeArray memo pair (findMin pair memo)) [(i,v) | i <- [1..(vertexCount - 1)], v <- [1..vertexCount]]
  return memo
  where
    findMin (i,v) memo =
      minimum ((Array.!) memo (i-1,v):map (\(t,w) -> (Array.!) memo (i-1,t) + w) ((IntMap.!) g v))
    resultList memo = map (\((_,v), d) -> (v,d)) . filter (\((i,_), _) -> i == vertexCount - 1 ) . Array.assocs $ memo
    resultArray memo = Array.array (1, vertexCount) (resultList memo)
-}
floydWarshall2 :: IntMap.IntMap [(Int, Double)] -> Int -> Either String (Array.Array (Int,Int) Double)
floydWarshall2 g vertexCount =
  if hasNegativeCycle then
    Left "Floyd-Warshall algorithm halted: negative cycle found"
  else
    Right result
  where
    memo = Array.array ((1,1,0), (vertexCount, vertexCount, vertexCount)) [findShortestPath (i,j,k) | i <- [1..vertexCount], j <- [1..vertexCount], k <- [0..vertexCount]]
    findShortestPath (i,j,k) =
      if k == 0 then
        if i == j then
          ((i,j,k), 0)
        else
          case filter (\(h,_) -> h == j) ((IntMap.!) g i) of
            [] -> ((i,j,k), 1/0)
            [(_,w)] -> ((i,j,k), w)
            _ -> error "Invalid graph"
      else
        ((i,j,k), minimum [(Array.!) memo (i,j,k-1), (Array.!) memo (i,k,k-1) + (Array.!) memo (k,j,k-1)])
    result = Array.array ((1,1), (vertexCount, vertexCount)) . map (\((i,j,_),w) -> ((i,j),w)) . filter (\((_,_,k), _) -> k == vertexCount) . Array.assocs $ memo
    hasNegativeCycle =
      any (<0) . map ((Array.!) memo) $ diagonals
        where
          diagonals = map (\x -> (x,x,vertexCount)) [1..vertexCount]

floydWarshall ::  Map.Map (Int,Int) Int ->Int -> Either String Int
floydWarshall g vertexCount = ST.runST $ do
  ht <- HT.newSized (vertexCount * vertexCount * vertexCount) :: ST.ST s (HashTable s (Int, Int, Int) Int)
  (v, ht') <- fwMemoF g ht (vertexCount, vertexCount, vertexCount)
  hasNegativeCycle' <- hasNegativeCycle vertexCount ht'
  if hasNegativeCycle' then
    return (Left "Floyd-Warshall algorithm halted: negative cycle found")
  else
    return (Right v)
  where
     hasNegativeCycle :: Int -> HashTable s (Int,Int,Int) Int -> ST.ST s Bool
     hasNegativeCycle n ht = fmap List.or (traverse (\d -> fmap (List.any (< 0)) (HT.lookup ht d)) diagonals)
       where
         diagonals = List.map (\x -> (x,x,n)) [1..n]

fwMemoF :: Map.Map (Int,Int) Int -> HashTable s (Int,Int,Int) Int -> (Int,Int,Int) -> ST.ST s (Int, HashTable s (Int,Int,Int) Int)
fwMemoF g ht (i,j,k) = do
  v <- HT.lookup ht (i,j,k)
  case v of
    Just v' -> return (v', ht)
    Nothing -> do
      (v',ht') <- floydWarshall' g ht (i,j,k)
      return (v',ht')

floydWarshall' :: Map.Map (Int,Int) Int -> HashTable s (Int,Int,Int) Int -> (Int,Int,Int) -> ST.ST s (Int, HashTable s (Int,Int,Int) Int)
floydWarshall' _ ht (i,j,0) | i == j = do
                                HT.insert ht (i,i,0) 0
                                return (0,ht)
floydWarshall' g ht (i,j,0) =
  case Map.lookup (i,j) g of
    Nothing -> do
      HT.insert ht (i,j,0) maxBound
      return (maxBound, ht)
    Just c -> do
      HT.insert ht (i,j,0) c
      return (c, ht)
floydWarshall' g ht (i,j,k) = do
  (val1, ht') <- fwMemoF g ht (i,j,k-1)
  (val2a, ht'') <- fwMemoF g ht' (i,j,k-1)
  (val2b, ht''') <- fwMemoF g ht'' (i,j,k-1)
  let val = min val1 (val2a + val2b)
  HT.insert ht''' (i,j,k) val
  return (val, ht''')

  {-

johnson :: Map.Map Int [(Int, Int)] -> Int -> Either String (Map.Map (Int,Int) Int)
johnson g vertexCount = do
  d' <- dijkstraResult
  ps <- weightAdjustments
  return $ Map.mapWithKey (\(u,v) distance -> distance - (Map.!) ps u + (Map.!) ps v) d'
  where
    bfResult = bellmanFord g vertexCount 0
    weightAdjustments = do
      bf <- bfResult
      let uvs = List.zip (repeat 0) [1..vertexCount]
          ps = Map.fromList (0,0:List.map (\(u,v) -> (v, (Map.!) bf (u,v))) uvs)
      return ps
    adjustedG = do
      ps <- weightAdjustments
      return (Map.mapWithKey (adjustedGStep ps) g)
    adjustedGStep ps key = List.map (\(h,w) -> (h,w + (Map.!) ps key - (Map.!) ps h))
    dijkstraResult :: Either String (Map.Map (Int, Int) Int)
    dijkstraResult = do
      g' <- adjustedG
      dijkstraResultsBySource <- traverse (dijkstra g' vertexCount) (Seq.fromList [0..vertexCount])
      Right $ Seq.foldlWithIndex (\acc k dr ->
                                    if k == 0 then
                                      acc
                                    else
                                      Map.foldlWithKey (\acc' k' d -> Map.insert (k,k') d acc') acc dr)
        Map.empty dijkstraResultsBySource

findShortestShortestPath :: Map.Map (Int,Int) Int -> Int
findShortestShortestPath = minimum . Map.elems
-}
