module Course2 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

type Graph = Map.Map Int [Int]

sccMain :: IO ()
sccMain = do
  file <- readFile "src/SCC.txt"
  let parsedList =  List.map (List.map read . words) . lines $ file :: [[Int]]
      baseG = Map.fromAscList (List.map (\k -> (k,[])) [1.. 875714]) :: Graph
      g = List.foldr (\x acc -> Map.insertWith (++) (List.head x) [List.last x] acc) baseG parsedList :: Graph
      revG = List.foldr (\x acc -> Map.insertWith (++) (List.last x) [List.head x] acc) baseG parsedList :: Graph
      firstRunResult =
        Map.foldrWithKey (\k v acc -> if Set.member k (fst acc) then acc else dfsForFinishingTime revG k acc) (Set.empty, []) revG
      (_, resultSccs, _) = List.foldl' (\(visited, sccs, leader) x -> if Set.member x visited then (visited, sccs, leader) else dfsForScc g x (visited, sccs, x)) (Set.empty, Map.empty, 0) (snd firstRunResult)
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

addIndex :: [a] -> [(Int, a)]
addIndex =
  List.reverse . fst . List.foldr (\x (acc, i) -> ((i, x) : acc, i + 1 )) ([], 0)
