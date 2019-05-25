module Course1.Week4 where

import qualified Data.List
import qualified Data.Set
import qualified Data.Map as M
import qualified System.Random


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

kargerMinCut' :: (M.Map Int [Int], System.Random.StdGen) -> IO (M.Map Int [Int], System.Random.StdGen)
kargerMinCut' (vertices, rndGen) =
  if M.size vertices <= 2 then
    pure (vertices, rndGen)
  else
    do
      kargerMinCut' . contractVertices $ (vertices, rndGen)

main :: Int -> IO ()
main runs = do
  file <- readFile "src/kargerMinCut.txt"
  let strings = Data.List.map words (lines file) :: [[String]]
  let ints = Data.List.map (Data.List.map read) strings :: [[Int]]
  let vertexMap = Data.List.foldl (\vAcc v -> M.insert (Data.List.head v) (Data.List.drop 1 v) vAcc) M.empty ints
  rndGen <- System.Random.getStdGen
  kargerMinCut runs vertexMap rndGen

kargerMinCut :: Int -> M.Map Int [Int] -> System.Random.StdGen -> IO ()
kargerMinCut runs vertexMap rndGen = do
  if runs == 0 then
    pure ()
  else do
    (resultMap, rndGen_) <- kargerMinCut' (vertexMap, rndGen)
    putStrLn $ "Run: " ++ show runs ++ ", Min cut: " ++ show (Data.List.length (resultMap M.! ((M.keys resultMap) !! 0)))
    kargerMinCut (runs - 1) vertexMap rndGen_
