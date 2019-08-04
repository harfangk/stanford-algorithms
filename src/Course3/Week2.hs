module Course3.Week2 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
  clustering1 <- readFile "src/Course3/clustering1.txt"
  clusteringBig <- readFile "src/Course3/clustering_big.txt"
  let (_, _, edges) = cluster 4 . sortEdges . List.foldl parseClustering1 (Map.empty, Map.empty, []) $ (lines clustering1)
      (_, bits, nodes) = List.foldl parseClustering2 (0, 0, Set.empty) $ (lines clusteringBig)
      clusterBig = cluster2 (buildBitMasks bits) (Set.toList nodes, Set.foldl (\acc n -> Map.insert n n acc) Map.empty nodes, Set.foldl (\acc n -> Map.insert n (Cluster n (Set.insert n Set.empty) Set.empty) acc) Map.empty nodes)
  print ("Answer to Q1: " ++ (show . getCost . List.head $ edges))
  print ("Answer to Q2: " ++ (show . List.length $ clusterBig))

-- Q1

type ClusterId = Int
type NodeId = Int

data Cluster = Cluster ClusterId (Set.Set NodeId) (Set.Set Edge)
  deriving (Show)
instance Eq Cluster where
  (==) (Cluster id1 _ _) (Cluster id2 _ _) = id1 == id2
instance Ord Cluster where
  compare (Cluster id1 _ _) (Cluster id2 _ _) = compare id1 id2

getId :: Cluster -> ClusterId
getId (Cluster id' _ _) = id'

getNodes :: Cluster -> Set.Set NodeId
getNodes (Cluster _ nodes _) = nodes

getEdges :: Cluster -> Set.Set Edge
getEdges (Cluster _ _ edges) = edges

data Edge = Edge Int Int Int
  deriving (Show)
instance Eq Edge where
  (==) (Edge tail1 head1 cost1) (Edge tail2 head2 cost2) = tail1 == tail2 && head1 == head2 && cost1 == cost2
instance Ord Edge where
  compare (Edge _ _ cost1) (Edge _ _ cost2) = compare cost1 cost2

getNodeIds :: Edge -> (NodeId, NodeId)
getNodeIds (Edge n1Id n2Id _) = (n1Id, n2Id)

getCost :: Edge -> Int
getCost (Edge _ _ cost) = cost

parseClustering1 :: (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster, [Edge]) -> String -> (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster, [Edge])
parseClustering1 (nodes, clusters, edges) s =
  case words s of
    [] ->
      (nodes, clusters, edges)
    [nodeCount] ->
      let
        size = read nodeCount :: Int
        range = [1 .. size]
        nodes' = Map.fromList . List.map (\i -> (i, i)) $ range
        clusters' = Map.fromList . List.map (\i -> ((i, Cluster i (Set.insert i Set.empty) Set.empty))) $ range
      in
        (nodes', clusters', edges)
    [n1, n2, c] ->
      (nodes, clusters, (edge : edges))
      where
        n1' = read n1 :: Int
        n2' = read n2 :: Int
        cost = read c :: Int
        edge = Edge n1' n2' cost
    _ ->
      (nodes, clusters, edges)

cluster :: Int -> (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster, [Edge]) -> (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster, [Edge])
cluster targetClusterCount (nodes, clusters, edges) =
  if List.null edges then
    (nodes, clusters, edges)
  else if (c1Id /= c2Id) && Map.size clusters <= targetClusterCount then
    (nodes, clusters, edges)
  else if (c1Id == c2Id) then
    cluster targetClusterCount (nodes, clusters, edges')
  else
    cluster targetClusterCount (nodes', clusters', edges')
    where
      edge = head edges
      edges' = tail edges
      (n1Id, n2Id) = getNodeIds edge
      c1Id = nodes Map.! n1Id
      c2Id = nodes Map.! n2Id
      c1 = clusters Map.! c1Id
      c2 = clusters Map.! c2Id
      (largerCluster, smallerCluster) =
        if (Set.size . getNodes $ c1) >= (Set.size . getNodes $ c2) then
          (c1, c2)
        else
          (c2, c1)
      clusters' = updateClusters largerCluster smallerCluster clusters
      nodes' = updateNodes (getId largerCluster) (getNodes smallerCluster) nodes

updateClusters :: Cluster -> Cluster -> Map.Map ClusterId Cluster -> Map.Map ClusterId Cluster
updateClusters largerCluster smallerCluster clusters =
  let
    c' = Cluster (getId largerCluster) (Set.union (getNodes largerCluster) (getNodes smallerCluster)) (Set.union (getEdges largerCluster) (getEdges smallerCluster))
    clusters' = Map.insert (getId largerCluster) c' clusters
    clusters'' = Map.delete (getId smallerCluster) clusters'
  in
    clusters''

updateNodes :: ClusterId -> Set.Set NodeId -> Map.Map NodeId ClusterId -> Map.Map NodeId ClusterId
updateNodes clusterId nodeIds nodes =
  Set.foldl' (\acc nodeId -> Map.insert nodeId clusterId acc) nodes nodeIds

sortEdges :: (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster, [Edge]) -> (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster, [Edge])
sortEdges (nodes, clusters, edges) = (nodes, clusters, List.sort edges)

-- Q2

parseClustering2 :: (Int, Int, Set.Set NodeId) -> String -> (Int, Int, Set.Set NodeId)
parseClustering2 (nodeCount, bits, nodes) s =
  case words s of
    [] ->
      (nodeCount, bits, nodes)
    [nc, b] ->
      (read nc, read b, nodes)
    bitString ->
      (nodeCount, bits, Set.insert (toDec . List.foldl (++) "" $ bitString) nodes)

cluster2 :: [Int] -> ([NodeId], Map.Map NodeId ClusterId, Map.Map ClusterId Cluster) -> Map.Map ClusterId Cluster
cluster2 bitMasks (nodeList, nodes, clusters) =
  if List.null nodeList then
    clusters
  else
    cluster2 bitMasks (nodeList', nodes', clusters')
    where
      node = head nodeList
      nodeList' = tail nodeList
      (nodes', clusters') = updateNodesAndClusters bitMasks node (nodes, clusters)

toDec :: String -> Int
toDec = List.foldl' (\acc x -> acc * 2 + Char.digitToInt x) 0

buildBitMasks :: Int -> [Int]
buildBitMasks n = zeroBitMasks ++ oneBitMasks ++ twoBitMasks
  where
    zeroBitMasks = [0]
    oneBitMasks = List.foldr (\x acc -> ((Bits.bit x):acc)) [] [0..(n - 1)] :: [Int]
    twoBitMasks = List.foldr (\x acc -> ((List.foldr (\y acc2 -> ((Bits.bit x) Bits..|. (Bits.bit y)):acc2) [] [0..(x-1)]) ++ acc)) [] [0..(n -1)] :: [Int]

updateNodesAndClusters :: [Int] -> Int -> (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster) -> (Map.Map NodeId ClusterId, Map.Map ClusterId Cluster)
updateNodesAndClusters bitMasks node (nodes, clusters) =
  (nodes', clusters'')
  where
    adjacentNodes = List.map (Bits.xor node) bitMasks :: [ClusterId]
    adjacentClusters = Maybe.catMaybes . List.map (\mbCid -> (>>=) mbCid (\cid -> Map.lookup cid clusters)) . List.map (\k -> Map.lookup k nodes) $ adjacentNodes :: [Cluster]
    largestCluster = List.maximumBy compareClustersBySize adjacentClusters
    largestClusterId = getId largestCluster
    targetClusters = List.filter (\c -> largestClusterId /= getId c) adjacentClusters
    nodesToUpdate = List.foldl (\acc c -> Set.union acc . getNodes $ c) Set.empty targetClusters :: Set.Set NodeId
    nodes' = Set.foldl' (\acc n -> Map.insert n largestClusterId acc) nodes nodesToUpdate :: Map.Map NodeId ClusterId
    c' = Cluster largestClusterId (Set.union (getNodes largestCluster) nodesToUpdate) Set.empty :: Cluster
    clusters' = Map.insert largestClusterId c' clusters :: Map.Map ClusterId Cluster
    clusters'' = List.foldl' (\acc c -> Map.delete (getId c) acc) clusters' targetClusters :: Map.Map ClusterId Cluster

compareClustersBySize :: Cluster -> Cluster -> Ordering
compareClustersBySize (Cluster _ nodes1 _) (Cluster _ nodes2 _) = compare (Set.size nodes1) (Set.size nodes2)
