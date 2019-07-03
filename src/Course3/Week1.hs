module Course3.Week1 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified MinHeap

main :: IO ()
main = do
  jobsFile <- readFile "src/Course3/jobs.txt"
  graphFile <- readFile "src/Course3/edges.txt"
  let jobs = List.foldl parseJobFolder [] (lines jobsFile)
      jobsByDiff = List.sortBy compareByDiff jobs
      jobsByRatio = List.sortBy compareByRatio jobs
      diffResult = List.foldr jobResultFolder (0,0) jobsByDiff
      ratioResult = List.foldr jobResultFolder (0,0) jobsByRatio
      g = List.foldl parseGraphFolder Map.empty (lines graphFile)
      mstWeightSum = prim g
  print ("diffResult: " ++ (show . snd $ diffResult))
  print ("ratioResult: " ++ (show . snd $ ratioResult))
  print ("mstWeightSum: " ++ (show mstWeightSum))

data Job = Job Int Int
  deriving (Show)

instance Eq Job where
  (Job w1 l1) == (Job w2 l2) = w1 == w2 && l1 == l2

parseJobFolder :: [Job] -> String -> [Job]
parseJobFolder acc s =
  case words s of
    [w, l] -> (Job (read w) (read l)) : acc
    _ -> acc

parseGraphFolder :: Map.Map Int (Set.Set Edge) -> String -> Map.Map Int (Set.Set Edge)
parseGraphFolder g s =
  case words s of
    [nodes,_] ->
      let
        size = read nodes :: Int
        range = [1 .. size]
        ls = List.map (\x -> (x, Set.empty)) range
      in
        Map.fromList ls
    [sv1, sv2, sw] ->
      Map.insert v2 v2es (Map.insert v1 v1es g)
      where
        v1 = read sv1 :: Int
        v2 = read sv2 :: Int
        w = Weight (read sw :: Int)
        e1 = Edge v1 v2 w
        e2 = Edge v2 v1 w
        v1es = Set.insert e1 ((Map.!) g v1)
        v2es = Set.insert e2 ((Map.!) g v2)
    _ ->
      g

jobResultFolder :: Job -> (Int, Int) -> (Int, Int)
jobResultFolder (Job w l) (c, wc) =
  ((c + l), wc + (w * (c + l)))

compareByDiff :: Job -> Job -> Ordering
compareByDiff (Job w1 l1) (Job w2 l2) =
    if d1 == d2 then
      compare w1 w2
    else
      compare d1 d2
    where
      d1 = w1 - l1
      d2 = w2 - l2

compareByRatio :: Job -> Job -> Ordering
compareByRatio (Job w1 l1) (Job w2 l2) =
  compare r1 r2
    where
      r1 = (fromIntegral w1 / fromIntegral l1) :: Double
      r2 = (fromIntegral w2 / fromIntegral l2) :: Double

data Edge = Edge Int Int Weight
  deriving (Show)

instance Eq Edge where
  (==) (Edge tail1 head1 w1) (Edge tail2 head2 w2) = tail1 == tail2 && head1 == head2 && w1 == w2
instance Ord Edge where
  compare (Edge _ _ w1) (Edge _ _ w2) = compare w1 w2

newtype Weight = Weight Int
  deriving (Show)
instance Eq Weight where
  (==) (Weight i1) (Weight i2) = i1 == i2
instance Ord Weight where
  compare (Weight i1) (Weight i2) = compare i1 i2

prim :: Map.Map Int (Set.Set Edge) -> Int
prim g =
  prim' g h 0
  where
    h = Map.foldlWithKey f MinHeap.empty g
    f :: MinHeap.MinHeap Weight Int -> Int -> (Set.Set Edge) -> MinHeap.MinHeap Weight Int
    f acc v _ =
      if v == 1 then
        MinHeap.insert (Weight (0), v) acc
      else
        MinHeap.insert (Weight (maxBound :: Int), v) acc

prim' :: Map.Map Int (Set.Set Edge) -> MinHeap.MinHeap Weight Int -> Int -> Int
prim' g h cost =
  if MinHeap.isEmpty h then
    cost
  else
    prim' g h'' cost'
  where
    ((Weight w), v, h') = MinHeap.extract h
    es = (Map.!) g v
    h'' = Set.foldl adjustHeap h' es
    cost' = cost + w

  -- Test this
adjustHeap :: MinHeap.MinHeap Weight Int -> Edge -> MinHeap.MinHeap Weight Int
adjustHeap h (Edge _ eHead weight) =
  case mbKv of
    Just (k, n) ->
      MinHeap.insert (min k weight, n) h'
    Nothing ->
      h'
  where
    (mbKv, h') = MinHeap.extractByV eHead h
