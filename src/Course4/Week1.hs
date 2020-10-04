{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Course4.Week1 where

import Control.Monad
import Data.Array (Array)
import Data.Coerce
import Data.Maybe
import Data.Semigroup
import System.IO

import qualified Data.Array.IArray as IArray
import qualified Data.Array.ST.Safe as STArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Set as Set

main :: IO ()
main = do
  (g1Vc, _, g1Edges) <- parseData "./src/Course4/test.txt"
  let g1 = buildAdjacencyListByTail g1Vc g1Edges
  let g1Result = floydWarshall2 g1 1
  print g1Result

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

inf :: Double
inf = 1/0

parseData :: FilePath -> IO (Int, Int, [(Int, Int, Double)])
parseData fp = withFile fp ReadMode $ \h -> do
  (vertexCount, edgeCount) <- getMetaData h
  edges <- replicateM edgeCount (getEdge h)
  pure (vertexCount, edgeCount, edges)
  where
    getMetaData h = parseMetaData <$> BS.hGetLine h
    parseMetaData line = fromJust $ do
      (x, line') <- BS.readInt line
      (y, line'') <- BS.readInt (dropSpace line')
      if BS.null (dropSpace line'')
        then pure (x, y)
        else error "Invalid data format"

    getEdge h = parseEdge <$> BS.hGetLine h
    parseEdge line = fromJust $ do
      (edgeTail, line') <- BS.readInt line
      (edgeHead, line'') <- BS.readInt (dropSpace line')
      (weight, line''') <- BS.readInteger (dropSpace line'')
      if BS.null (dropSpace line''')
        then pure (edgeTail, edgeHead, fromInteger weight)
        else error "Invalid data format"

    dropSpace = BS.dropWhile (== ' ')

addExtraVertexForJohnson :: (Int, Int, [(Int, Int, Int)]) -> (Int, Int, [(Int, Int, Int)])
addExtraVertexForJohnson (vertexCount, edgeCount, edges) =
  (vertexCount', edgeCount', edges')
  where
    vertexCount' = vertexCount + 1
    edgeCount' = edgeCount + vertexCount
    newEdges = List.map (0,, 0) [1..vertexCount]
    edges' = edges ++ newEdges

buildAdjacencyListByTail :: (Fractional a, Ord a) => Int -> [(Int, Int, a)] -> Array Int [(Int, a)]
buildAdjacencyListByTail vC =
  IArray.listArray (1, vC)
  . IntMap.elems
  . List.foldl' (\acc (t, h, w) -> IntMap.adjust ((h, w) :) t acc) initialMap
  where
    initialMap = IntMap.fromAscList (List.zip [1 .. vC] (List.repeat []))

buildAdjacencyListByHead :: (Fractional a, Ord a) => Int -> [(Int, Int, a)] -> Array Int [(Int, a)]
buildAdjacencyListByHead vC =
  IArray.listArray (1, vC)
  . IntMap.elems
  . List.foldl' (\acc (t, h, w) -> IntMap.adjust ((t, w) :) h acc) initialMap
  where
    initialMap = IntMap.fromAscList (List.zip [1 .. vC] (List.repeat []))

dijkstra :: (STArray.Ix v, Num w, Ord w, Bounded w, Fractional w) => Array v [(v, w)] -> v -> Either String (Array v w)
dijkstra g s =
    if List.any ((>) 0 . snd) . List.concat . IArray.elems $ g then
      Left "Dijkstra's algorithm halted: negative edge found"
    else
      Right $ STArray.runSTArray $ do
        dists <- STArray.newArray (IArray.bounds g) (1/0)
        STArray.writeArray dists s 0
        let dijkstra'' queue =
              case Set.minView queue of
                Nothing -> return ()
                Just ((distU, u), queue') ->
                  let edges = g IArray.! u
                      f q (v, weight) = do
                        let distV' = distU + weight
                        distV <- STArray.readArray dists v
                        if distV <= distV' then
                          return q
                        else do
                          let q' = Set.delete (distV, v) q
                          STArray.writeArray dists v distV'
                          return $ Set.insert (distV', v) q'
                  in
                    foldM f queue' edges >>= dijkstra''
        dijkstra'' (Set.singleton (0, s))
        return dists

bellmanFord :: Array Int [(Int, Double)] -> Int -> Int -> Either String (Array Int Double)
bellmanFord g vC s =
  if hasNegativeCycle then
    Left "Bellman-Ford algorithm halted: negative cycle found"
  else
    Right resultArray
  where
    calculated = STArray.runSTUArray $ do
      array <- STArray.newArray (1, vC) inf
      STArray.writeArray array s 0
      relaxEdge array 1 1
      where
        relaxEdge array edgeCount v
          | v > vC =
            if edgeCount < vC
            then relaxEdge array (edgeCount + 1) 1
            else pure array
          | otherwise = do
            forM_ (g IArray.! v) $ coreCmp array v
            relaxEdge array edgeCount (v + 1)

        coreCmp array v (f, w) = do
          vW <- STArray.readArray array v
          fW <- STArray.readArray array f
          STArray.writeArray array v (min (fW + w) vW)
        {-# INLINE coreCmp #-}

    resultList = IArray.elems calculated
    resultArray = IArray.listArray (1, vC) resultList

    getResultOf = (resultArray IArray.!)
    {-# INLINE getResultOf #-}

    hasNegativeCycle =
      coerce
      . foldMap (\(t, efs) -> foldMap (\(f, w) -> coerce (getResultOf f + w < getResultOf t) :: Any) efs)
      $ IArray.assocs g

floydWarshall2 :: Array Int [(Int, Double)] -> Int -> Either String (Array (Int,Int) Double)
floydWarshall2 g vC =
  if hasNegativeCycle then
    Left "Floyd-Warshall algorithm halted: negative cycle found"
  else
    Right resultArray
  where
    calculated = STArray.runSTUArray $ do
      arr <- STArray.newArray ((1,1,0), (vC, vC, vC)) 0
      let ixs = [(i,j) | i <- [1..vC], j <- [1..vC]]
      forM_ ixs $ \(i,j) ->
          if i == j then
            STArray.writeArray arr (i,j,0) 0
          else
            case List.filter (\(h,_) -> h == j) (g IArray.! i) of
              [] -> STArray.writeArray arr (i,j,0) inf
              [(_,w')] -> STArray.writeArray arr (i,j,0) w'
              _ -> error "Invalid case"
      forM_ [1..vC] (\k ->
        forM_ [1..vC] (\i ->
          forM_ [1..vC] (\j -> do
            case1 <- STArray.readArray arr (i,j,k-1)
            case2a <- STArray.readArray arr (i,k,k-1)
            case2b <- STArray.readArray arr (k,j,k-1)
            STArray.writeArray arr (i,j,k) (min case1 (case2a + case2b))
                        )
                      )
                    )
      return arr

    resultArray = IArray.array ((1,1), (vC,vC)) . map (\((i,j,_),c) -> ((i,j),c)) . filter (\((_,_,k),_) -> k == vC) . IArray.assocs $ calculated

    hasNegativeCycle =
      any (<0) . map ((IArray.!) calculated) $ diagonals
        where
          diagonals = map (\x -> (x,x,vC)) [1..vC]
{-
      where
    memo = IArray.array ((1,1,0), (vertexCount, vertexCount, vertexCount)) [findShortestPath (i,j,k) | i <- [1..vertexCount], j <- [1..vertexCount], k <- [0..vertexCount]]
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
floydWarshall2 :: Array Int [(Int, Double)] -> Int -> Either String (Array (Int,Int) Double)
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
-}
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
